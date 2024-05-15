/*-
 * Copyright (c) 2023 Peter Rugg
 * Copyright (c) 2022-2024 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package AXI4WidthsExample;

import SourceSink :: *;
import BlueAXI4 :: *;
import BlueUtils :: *;
import BlueBasics :: *;
import Connectable :: *;
import Vector :: *;
import FIFOF :: *;
import FIFO :: *;
import List :: *;
import GetPut :: *;
import Recipe :: *;

// helpers
////////////////////////////////////////////////////////////////////////////////

Integer verbosity_level = 6;
function Action vPrint (Integer lvl, Fmt fmt) =
  action if (verbosity_level >= lvl) $display ("%0t - ", $time, fmt); endaction;

function Action die (Fmt fmt) = action
  $display ("%0t - ", $time, fmt);
  $finish;
endaction;

module mkRNG #(a seed) (Source #(a)) provisos (Bits #(a, sz), Arith #(a));
  let state <- mkReg (seed);
  method canPeek = True;
  method peek = state;
  method drop = action state <= state * 6364136223846793005 + 1; endaction;
endmodule

module mkListToSource #(List #(t) xs) (Source #(Maybe #(t)))
  provisos (Bits #(t, _));
  Bit #(32) xsLen = fromInteger (length (xs));
  Reg #(Bit #(32)) i <- mkReg (0);
  let ff <- mkFIFOF;
  rule produce (i <= xsLen);
    ff.enq ((i == xsLen) ? Invalid : Valid (xs[i]));
    i <= i + 1;
  endrule
  return toSource (ff);
endmodule

function Bit #(t_data) getRelevantData ( Bit #(t_data) data
                                       , AXI4_Len flitIdx
                                       , AXI4_Size size
                                       , Bit #(6) offset );
  Bit #(6) offsetMask = ~0 << pack (size);
  Bit #(6) alignedOffset = offset & offsetMask;
  Bit #(6) withinSizeOffset = offset & ~offsetMask;
  Bit #(t_data) mask = ~(~0 << (1 << (pack (size) + 3)));
  if (flitIdx == 0) mask = mask & (~0 << {withinSizeOffset, 3'b000});

  Bit #(9) byteShftAmnt =
    zeroExtend (alignedOffset) + (1 << pack (size)) * zeroExtend (flitIdx);
  Bit #(t_data) shiftedData = data >> (byteShftAmnt << 3);
  return shiftedData & mask;
endfunction

module writeAXI4_Slave #(
    Bit #(t_addr) start_addr
  , Tuple3 #(Bit #(n), AXI4_Size, AXI4_Len) params
  , Bit #(t_all_data) all_data
  , AXI4_Slave #( t_id, t_addr, t_data
                , t_awuser, t_wuser, t_buser
                , t_aruser, t_ruser ) slv
  ) (RecipeFSM) provisos (
    NumAlias #(t_ratio, TDiv #(t_all_data, t_data))
  , Mul #(t_ratio, t_data, t_all_data)
  , Add #(_a, TLog #(t_ratio), t_addr)
  , Add #(_b, TLog #(TDiv #(t_data, 8)), t_addr)
  , Add #(_c, TAdd #(SizeOf #(AXI4_Len), 1), t_addr)
  , Add #(_d, n, t_addr)
  , Add #(_e, TLog #(TDiv #(t_data, 8)), 8)
  , Add #(_f, 8, t_addr)
  );
  Reg #(Bit #(TAdd #(SizeOf #(AXI4_Len), 1))) flitCnt <- mkRegU;
  match {.offset, .size, .len} = params;
  // state machine
  let fsm <- mkRecipeFSM (rSeq (rBlock (
    action
      vPrint(4, $format("writeAXI4_Slave - resetting"));
      flitCnt <= 0;
      if (fromAXI4_Size (size) > fromInteger (valueOf(t_data) / 8))
        die ($format ("writeAXI4_Slave: invalid AXI4 size ", fshow (size)));
    endaction
  , action
      AXI4_AWFlit #(t_id, t_addr, t_awuser) awflit = AXI4_AWFlit {
        awaddr: start_addr + zeroExtend (offset)
      , awid: 0
      , awsize: size
      , awlen: len
      , awregion: 0
      , awburst: INCR
      , awprot: 0
      , awcache: 0
      , awlock: ?
      , awqos: 0
      , awuser: 0
      };
      slv.aw.put (awflit);
      vPrint(5, $format("writeAXI4_Slave - sent ", fshow (awflit)));
    endaction
  , rWhile (flitCnt <= zeroExtend (len),
      rAct (action
        Bit #(t_addr) currentAddr =
          start_addr + zeroExtend (offset) + (zeroExtend (flitCnt) << pack (size));
        Bit #(TLog #(t_ratio)) lanes_idx =
          truncate (currentAddr >> log2 (valueOf (t_data) / 8));
        Bit #(t_addr) currentAlignedAddr =
          (currentAddr >> log2 (valueOf (t_data) / 8)) << log2 (valueOf (t_data) / 8);
        Vector #(t_ratio, Bit #(t_data)) data = unpack (all_data);
        Bit #(TLog #(TDiv #(t_data, 8))) byteOffset = truncate (currentAlignedAddr);
        // prepare byte strobe and check for unaligned accesses, fix first flit
        Bit #(8) accessSize = (1 << pack (size));
        if (flitCnt == 0) accessSize =
          accessSize - truncate (currentAddr & ~(~0 << pack (size)));
        Bit #(TDiv #(t_data, 8)) strb = ~(~0 << accessSize);
        AXI4_WFlit #(t_data, t_wuser) wflit = AXI4_WFlit {
          wdata: data[lanes_idx]
        , wstrb: strb << byteOffset
        , wlast: flitCnt == zeroExtend (len)
        , wuser: 0
        };
        slv.w.put (wflit);
        flitCnt <= flitCnt + 1;
        vPrint(5, $format("writeAXI4_Slave - sent ", fshow (wflit)));
        vPrint(6, $format( "writeAXI4_Slave "
                         , "- flitCnt <= ", fshow (flitCnt + 1) ));
      endaction)
    )
  , rWhile (!slv.b.canPeek, rAct (action
      vPrint(5, $format("writeAXI4_Slave - wait for b flit"));
    endaction))
  , action
      let bflit <- get (slv.b);
      vPrint(5, $format("writeAXI4_Slave - consume b flit: ", fshow (bflit)));
    endaction
  )));
  return fsm;
endmodule

////////////////////////////////////////////////////////////////////////////////

module mkReadStimuliFSM #( AXI4_Slave #( t_id, t_addr, t_data
                                       , t_awuser, t_wuser, t_buser
                                       , t_aruser, t_ruser ) golden
                         , AXI4_Slave #( t_id, t_addr, t_data
                                       , t_awuser, t_wuser, t_buser
                                       , t_aruser, t_ruser ) dut
                         , Source #(Maybe #(Tuple3 #( Bit #(6)
                                                    , AXI4_Size
                                                    , AXI4_Len ))) paramSrc
                         )
  (RecipeFSM)
  provisos (Add #(__a, 6, t_addr));
  // bookkeepig registers and fifos
  //req side
  Reg #(Bool) reqDone <- mkRegU;
  // rsp side
  Reg #(AXI4_Len)  rspFlitIdx <- mkRegU;
  Reg #(Bool)         rspDone <- mkRegU;
  Reg #(Bool)         allDone <- mkRegU;
  FIFOF #(Maybe #(Tuple3 #(AXI4_Len, AXI4_Size, Bit #(6)))) ff <- mkFIFOF;
  // state machine
  let fsm <- mkRecipeFSM (rSeq(rBlock(
    // init bookkeeping
    action
      reqDone <= False;
      rspFlitIdx <= 0;
      ff.clear;
      rspDone <= False;
      allDone <= False;
      vPrint(4, $format("mkReadStimuliFSM - resetting"));
    endaction
  , rPar(rBlock(
      // send requests
      rSeq(rBlock(
        rWhile (!reqDone, rAct(action
          let mParams <- get (paramSrc);
          case (mParams) matches
            tagged Invalid: begin
              ff.enq(Invalid);
              reqDone <= True;
              vPrint(2, $format("mkReadStimuliFSM - done sending requests"));
            end
            tagged Valid {.reqOffset, .reqSize, .reqLen}: begin
              AXI4_ARFlit #(t_id, t_addr, t_aruser) arflit = AXI4_ARFlit {
                araddr: zeroExtend (reqOffset)
              , arid: 0
              , aruser: 0
              , arlen: reqLen
              , arburst: INCR
              , arcache: 0
              , arlock: ?
              , arregion: 0
              , arqos: 0
              , arprot: 0
              , arsize: reqSize
              };
              golden.ar.put(arflit);
              dut.ar.put(arflit);
              ff.enq (Valid (tuple3 (reqLen, reqSize, reqOffset)));
              vPrint(2, $format( "mkReadStimuliFSM - sent (to golden and dut) "
                               , fshow(arflit) ));
              //vPrint(2, $format( "mkReadStimuliFSM - ff.enq Valid"
              //                 , " reqLen: %0d", reqLen
              //                 , " reqSize: %0d", reqSize
              //                 , " reqOffset: %0d", reqOffset ));
            end
          endcase
        endaction))
      , rAct(action
          ff.enq(Invalid);
          vPrint(2, $format("mkReadStimuliFSM - done sending requests"));
        endaction)
      ))
      // receive responses
    , rWhile (!allDone, rWhen (ff.notEmpty, rIfElse (
        isValid (ff.first)
      , rSeq(rBlock(
          rAct(action rspDone <= False; endaction)
        , rWhile (!rspDone, rAct(action
            match {.fflen, .ffsize, .ffoffset} = ff.first.Valid;
            let goldenR <- get (golden.r);
            let goldenData =
              getRelevantData (goldenR.rdata, rspFlitIdx, ffsize, ffoffset);
            let dutR <- get (dut.r);
            let dutData =
              getRelevantData (dutR.rdata, rspFlitIdx, ffsize, ffoffset);
            $display("------------------------------------------------");
            vPrint (1, $format( "rspFlitIdx: ", fshow (rspFlitIdx)
                              , ", fflen: ", fshow (fflen)
                              , ", ffsize: ", fshow (ffsize)
                              , ", ffoffset: ", fshow (ffoffset)
                              ));
            vPrint (1, $format("golden RFlit: ", fshow (goldenR)));
            vPrint (1, $format("dut RFlit   : ", fshow (dutR)));
            vPrint (1, $format("golden data: ", fshow (goldenData)));
            vPrint (1, $format("dut data   : ", fshow (dutData)));
            if (dutData != goldenData) die ($format("Fail"));
            if (goldenR.rlast) begin
              rspDone <= True;
              ff.deq;
              rspFlitIdx <= 0;
              vPrint (2, $format( "mkReadStimuliFSM -  ff.deq"
                                , "reset rspFlitIdx" ));
            end else rspFlitIdx <= rspFlitIdx + 1;
          endaction))
        ))
      , rAct (action
          allDone <= True;
          ff.deq;
          vPrint (2, $format( "mkReadStimuliFSM "
                            , "-  ff.first ", fshow(ff.first) ));
          vPrint (2, $format( "mkReadStimuliFSM "
                            , "-  final dequeue, allDone <= True" ));
        endaction)
      )))
    ))
    // all req / rsp pairs have gone through, success
  , die ($format("Success"))
  )));
  return fsm;
endmodule

module mkWriteStimuliFSM #( AXI4_Slave #( t_id, t_addr, t_data
                                        , t_awuser, t_wuser, t_buser
                                        , t_aruser, t_ruser ) golden
                          , AXI4_Slave #( t_id, t_addr, t_data
                                        , t_awuser, t_wuser, t_buser
                                        , t_aruser, t_ruser ) dut
                          , Source #(Maybe #(Tuple3 #( Bit #(6)
                                                     , AXI4_Size
                                                     , AXI4_Len ))) paramSrc
                          , Source #(Bit #(512)) randSrc
                          )
  (RecipeFSM)
  provisos ( Mul#(TDiv#(512, t_data), t_data, 512)
           , Add#(a__, TLog#(TDiv#(512, t_data)), t_addr)
           , Add#(b__, TLog#(TDiv#(t_data, 8)), 8)
           , Add#(c__, TLog#(TDiv#(t_data, 8)), t_addr)
           , Add#(d__, 6, t_addr)
           , Add#(e__, 8, t_addr)
           , Add#(f__, TAdd#(SizeOf #(AXI4_Len), 1), t_addr)
           );
  RecipeFSM goldenWrite <-
    writeAXI4_Slave (0, paramSrc.peek.Valid, randSrc.peek, golden);
  RecipeFSM dutWrite <-
    writeAXI4_Slave (0, paramSrc.peek.Valid, randSrc.peek, dut);
  let allDone <- mkReg (False);
  let flitIdx <- mkRegU;
  let checkRspDone <- mkRegU;
  let fsm <- mkRecipeFSM (rSeq (rBlock (
    rWhile (!allDone, rSeq (rBlock (
    action vPrint (1, $format("=======================================")); endaction
    , action vPrint (1, $format("==== write to golden ==== - ", fshow (paramSrc.peek))); endaction
    , goldenWrite.trigger
    , rWhile(!goldenWrite.canTrigger, rAct(noAction))
    , action vPrint (1, $format("==== write to dut ==== - ", fshow (paramSrc.peek))); endaction
    , dutWrite.trigger
    , rWhile(!dutWrite.canTrigger, rAct(noAction))
    , action vPrint (1, $format("==== send read back ====")); endaction
    , action
        match {.reqOffset, .reqSize, .reqLen} = paramSrc.peek.Valid;
        AXI4_ARFlit #(t_id, t_addr, t_aruser) arflit = AXI4_ARFlit {
            araddr: zeroExtend (reqOffset)
          , arid: 0
          , aruser: 0
          , arlen: reqLen
          , arburst: INCR
          , arcache: 0
          , arlock: ?
          , arregion: 0
          , arqos: 0
          , arprot: 0
          , arsize: reqSize
          };
          golden.ar.put(arflit);
          dut.ar.put(arflit);
          vPrint (1, $format("AR to golden and dut: ", fshow (arflit)));
          flitIdx <= 0;
          checkRspDone <= False;
      endaction
    , rWhile (!checkRspDone, rAct (action
        match {.reqOffset, .reqSize, .reqLen} = paramSrc.peek.Valid;
        let goldenR <- get (golden.r);
        let dutR <- get (dut.r);
        let goldenData = getRelevantData ( goldenR.rdata
                                         , flitIdx
                                         , reqSize
                                         , reqOffset );
        let dutData = getRelevantData ( dutR.rdata
                                      , flitIdx
                                      , reqSize
                                      , reqOffset );
        $display("------------------------------------------------");
        vPrint (1, $format( "flitIdx: ", fshow (flitIdx)
                          , ", reqOffset: ", fshow (reqOffset)
                          , ", reqSize: ", fshow (reqSize)
                          , ", reqLen: ", fshow (reqLen)
                          ));
        vPrint (1, $format("golden RFlit: ", fshow (goldenR)));
        vPrint (1, $format("dut RFlit   : ", fshow (dutR)));
        vPrint (1, $format("golden data: ", fshow (goldenData)));
        vPrint (1, $format("dut data   : ", fshow (dutData)));
        if (dutData != goldenData) die ($format ("Fail"));
        flitIdx <= flitIdx + 1;
        if (goldenR.rlast) begin
          paramSrc.drop;
          randSrc.drop;
          checkRspDone <= True;
        end
      endaction))
    , action
        if (paramSrc.canPeek && !isValid (paramSrc.peek))
          allDone <= True;
      endaction
    )))
    // all req / rsp pairs have gone through, success
  , die ($format ("Success"))
  )));
  return fsm;
endmodule

////////////////////////////////////////////////////////////////////////////////

// mem setup list
List #(Bit #(32)) mem_setup_values = list (
  'h01234567
, 'h89abcdef
, 'h02468ace
, 'h13579bdf
, 'h048c26ae
, 'h159d37bf
, 'h082a4c6e
, 'h193b5d7f
, 'hfedcba98
, 'h76543210
, 'h00000001
, 'h11111112
, 'h22222223
, 'h33333334
, 'hfedcba98
, 'h76543210
);

Bit #(512) mem_setup_value = 512'h01234567_89abcdef_02468ace_13579bdf_048c26ae_159d37bf_082a4c6e_193b5d7f_fedcba98_76543210_00000001_11111112_22222223_33333334_fedcba98_76543210;

// flit parameters to test
function List #(Tuple3 #(Integer, Integer, Integer))
  genFlitParams (Integer memBytes, Integer maxByteWidth);
  List #(Tuple3 #(Integer, Integer, Integer)) flitParams = Nil;
  for (Integer offset = 0; offset < memBytes; offset = offset + 1) begin
    Integer remain = memBytes - offset;
    Integer maxSize = min (2**log2 (remain), maxByteWidth);
    for (Integer size = 1; size <= maxSize; size = size * 2) begin
      Integer maxLen = remain / size;
      for (Integer len = 1; len <= maxLen; len = len + 1) begin
        flitParams = List::cons (tuple3 (offset, size, len), flitParams);
      end
    end
  end
  return flitParams;
endfunction

// turn flit parameter Integers to AXI4-friendly types
function Tuple3 #(Bit #(n), AXI4_Size, AXI4_Len)
         toAXI4_Params (Tuple3 #(Integer, Integer, Integer) params);
  match {.offset, .size, .len} = params;
  return tuple3 ( fromInteger (offset)
                , fromInteger (size)
                , fromInteger (len - 1) );
endfunction

// test reads from wide interface to narrow interface
////////////////////////////////////////////////////////////////////////////////
module testReadsWideToNarrow (Empty);
  // golden memory
  AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0) goldenMem <- mkAXI4Mem (512, UnInit);
  let setupGolden <- writeAXI4_Slave (
      0
    , tuple3 (8'h0, toAXI4_Size (64).Valid, 0)
    , mem_setup_value
    , goldenMem
    );
  // dut memory
  AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0) dutMem <- mkAXI4Mem (512, UnInit);
  NumProxy#(1) one = ?;
  Tuple2 #( AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0)
          , AXI4_Master#(0, 16, 64, 0, 0, 0, 0, 0) )
    wide2narrow <- mkAXI4DataWidthShim_WideToNarrow (one, one);
  match {.dutSlave, .dutMaster} = wide2narrow;
  mkConnection (dutMaster, dutMem);
  let setupDUT <- writeAXI4_Slave (
      0
    , tuple3 (8'h0, toAXI4_Size (64).Valid, 0)
    , mem_setup_value
    , dutSlave
    );
  // run test
  let reqParams =
    List::map (toAXI4_Params, genFlitParams (valueOf(512)/8, valueOf(512)/8));
  let paramSrc <- mkListToSource (reqParams);
  let stimuli <- mkReadStimuliFSM (goldenMem, dutSlave, paramSrc);
  let fsm <- mkRecipeFSM (rSeq(rBlock(
    $display("%0t - running setupGolden", $time)
  , setupGolden.trigger
  , rWhile(!setupGolden.canTrigger, rAct(noAction))
  , $display("%0t - running setupDUT", $time)
  , setupDUT.trigger
  , rWhile(!setupDUT.canTrigger, rAct(noAction))
  , $display("%0t - running stimuli", $time)
  , stimuli.trigger
  )));
  let once <- mkReg(True);
  rule startTest(once); fsm.trigger; once <= False; endrule
endmodule

// test reads from narrow interface to wide interface
////////////////////////////////////////////////////////////////////////////////
module testReadsNarrowToWide (Empty);
  // golden memory
  AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0) goldenMem <- mkAXI4Mem (512, UnInit);
  let setupGolden <- writeAXI4_Slave (
      0
    , tuple3 (8'h0, toAXI4_Size (8).Valid, 7)
    , mem_setup_value
    , goldenMem
    );
  // dut memory
  AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0) dutMem <- mkAXI4Mem (512, UnInit);
  NumProxy#(1) one = ?;
  Tuple2 #( AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0)
          , AXI4_Master#(0, 16, 512, 0, 0, 0, 0, 0) )
    narrow2wide <- mkAXI4DataWidthShim_NarrowToWide (one, one);
  match {.dutSlave, .dutMaster} = narrow2wide;
  mkConnection (dutMaster, dutMem);
  let setupDUT <- writeAXI4_Slave (
      0
    , tuple3 (8'h0, toAXI4_Size (8).Valid, 7)
    , mem_setup_value
    , dutSlave
    );
  // run test
  let reqParams =
    List::map (toAXI4_Params, genFlitParams (valueOf(512)/8, valueOf(64)/8));
  let paramSrc <- mkListToSource (reqParams);
  let stimuli <- mkReadStimuliFSM (goldenMem, dutSlave, paramSrc);
  let fsm <- mkRecipeFSM (rSeq(rBlock(
    $display("%0t - running setupGolden", $time)
  , setupGolden.trigger
  , rWhile(!setupGolden.canTrigger, rAct(noAction))
  , $display("%0t - running setupDUT", $time)
  , setupDUT.trigger
  , rWhile(!setupDUT.canTrigger, rAct(noAction))
  , $display("%0t - running stimuli", $time)
  , stimuli.trigger
  )));
  let once <- mkReg(True);
  rule startTest(once); fsm.trigger; once <= False; endrule
endmodule

// test writes from wide interface to narrow interface
module testWritesWideToNarrow (Empty);
  // golden memory
  AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0) goldenMem <- mkAXI4Mem (512, UnInit);
  // dut memory
  AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0) dutMem <- mkAXI4Mem (512, UnInit);
  NumProxy#(1) one = ?;
  Tuple2 #( AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0)
          , AXI4_Master#(0, 16, 64, 0, 0, 0, 0, 0) )
    wide2narrow <- mkAXI4DataWidthShim_WideToNarrow (one, one);
  match {.dutSlave, .dutMaster} = wide2narrow;
  mkConnection (dutMaster, dutMem);
  // random number generator
  Source #(Bit #(512)) randSrc <- mkRNG (512'h01234567_89abcdef_02468ace_13579bdf_048c26ae_159d37bf_082a4c6e_193b5d7f_fedcba98_76543210_00000001_11111112_22222223_33333334_44444445_55555556);
  let reqParams =
    List::map (toAXI4_Params, genFlitParams (valueOf(512)/8, valueOf(512)/8));
  let paramSrc <- mkListToSource (reqParams);
  let fsm <- mkWriteStimuliFSM (goldenMem, dutSlave, paramSrc, randSrc);
  let once <- mkReg(True);
  rule startTest(once); fsm.trigger; once <= False; endrule
endmodule

endpackage
