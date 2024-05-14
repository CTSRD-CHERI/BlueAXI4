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

module mkRNG #(a seed) (Get #(a)) provisos (Bits #(a, sz), Arith #(a));
  let state <- mkReg (seed);
  method get = actionvalue
    let newState = state * 6364136223846793005 + 1;
    state <= newState;
    return newState;
  endactionvalue;
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

////////////////////////////////////////////////////////////////////////////////

module writeAXI4_Slave #(
    List #(Bit #(n)) all_data
  , Bit #(t_addr) start_addr
  , AXI4_Slave #( t_id, t_addr, t_data
                , t_awuser, t_wuser, t_buser
                , t_aruser, t_ruser ) slv
  ) (RecipeFSM) provisos (
    Add #(_a, n, t_data)
  , Add #(_b, TLog #(TDiv #(t_data, n)), t_addr)
  );

  Reg #(Bit #(t_addr)) flitIdx <- mkReg (0);
  Source #(Maybe #(Bit #(n))) dataSrc <- mkListToSource (all_data);
  Integer nBits = valueOf (n);
  Integer nBytes = nBits / 8;
  let fsm <- mkRecipeFSM (rSeq(rBlock(
      rAct (action
        AXI4_AWFlit #(t_id, t_addr, t_awuser) awflit = AXI4_AWFlit {
            awaddr: start_addr
          , awid: 0
          , awsize: fromInteger (nBytes)
          , awlen: fromInteger (length (all_data))
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
      endaction)
    , rWhile (flitIdx < fromInteger (length (all_data)), rSeq (rBlock (
        rWhile (!dataSrc.canPeek, rAct (
          vPrint(6, $format("writeAXI4_Slave - waiting for data to send"))
        ))
      , rWhen (isValid (dataSrc.peek),
          rAct (action
            Maybe #(Bit #(n)) data <- get (dataSrc);
            Bit #(t_addr) addr = start_addr + (flitIdx << log2 (nBytes));
            Bit #(TLog #(TDiv #(t_data, n))) lanes_idx =
              truncate (addr >> log2 (nBytes));
            Bit #(t_addr) wide_lanes_idx = zeroExtend (lanes_idx);
            AXI4_WFlit #(t_data, t_wuser) wflit = AXI4_WFlit {
                wdata:
                  zeroExtend (data.Valid) << (fromInteger (nBits) * wide_lanes_idx)
              , wstrb:
                  ~(~0 << nBytes) << (fromInteger (nBytes) * wide_lanes_idx)
              , wlast:
                  flitIdx == fromInteger (length (all_data) - 1)
              , wuser: 0
              };
            slv.w.put (wflit);
            flitIdx <= flitIdx + 1;
            vPrint(5, $format("writeAXI4_Slave - sent ", fshow (wflit)));
            vPrint(6, $format( "writeAXI4_Slave "
                             , "- flitIdx <= ", fshow (flitIdx + 1) ));
          endaction))
      )))
    , rWhile (!slv.b.canPeek, rAct(
        vPrint(6, $format("writeAXI4_Slave - waiting for b flit"))
      ))
    , rAct(action
        let bflit <- get (slv.b);
        vPrint(6, $format("writeAXI4_Slave - received ", fshow (bflit)));
      endaction)
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
  // helper function
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
        flitParams = cons (tuple3 (offset, size, len), flitParams);
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
module testReadWideToNarrow (Empty);
  // golden memory
  AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0) goldenMem <- mkAXI4Mem (512, UnInit);
  let setupGolden <- writeAXI4_Slave (mem_setup_values, 0, goldenMem);
  // dut memory
  AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0) dutMem <- mkAXI4Mem (512, UnInit);
  NumProxy#(1) one = ?;
  Tuple2 #( AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0)
          , AXI4_Master#(0, 16, 64, 0, 0, 0, 0, 0) )
    wide2narrow <- mkAXI4DataWidthShim_WideToNarrow (one, one);
  match {.dutSlave, .dutMaster} = wide2narrow;
  mkConnection (dutMaster, dutMem);
  let setupDUT <- writeAXI4_Slave (mem_setup_values, 0, dutSlave);
  // run test
  let reqParams =
    map (toAXI4_Params, genFlitParams (valueOf(512)/8, valueOf(512)/8));
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
module testReadNarrowToWide (Empty);
  // golden memory
  AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0) goldenMem <- mkAXI4Mem (512, UnInit);
  let setupGolden <- writeAXI4_Slave (mem_setup_values, 0, goldenMem);
  // dut memory
  AXI4_Slave #(0, 16, 512, 0, 0, 0, 0, 0) dutMem <- mkAXI4Mem (512, UnInit);
  NumProxy#(1) one = ?;
  Tuple2 #( AXI4_Slave #(0, 16, 64, 0, 0, 0, 0, 0)
          , AXI4_Master#(0, 16, 512, 0, 0, 0, 0, 0) )
    narrow2wide <- mkAXI4DataWidthShim_NarrowToWide (one, one);
  match {.dutSlave, .dutMaster} = narrow2wide;
  mkConnection (dutMaster, dutMem);
  let setupDUT <- writeAXI4_Slave (mem_setup_values, 0, dutSlave);
  // run test
  let reqParams =
    map (toAXI4_Params, genFlitParams (valueOf(512)/8, valueOf(64)/8));
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

endpackage
