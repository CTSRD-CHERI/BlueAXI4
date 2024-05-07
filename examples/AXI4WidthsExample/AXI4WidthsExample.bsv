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

////////////////////////////////////////////////////////////////////////////////
module mkSlaveSetup #(AXI4_Slave #( t_id, t_addr, t_data
                                  , t_awuser, t_wuser, t_buser
                                  , t_aruser, t_ruser ) slv) (RecipeFSM)
  provisos (Add #(__a, t_data, 512));
  Reg #(Bit #(512))    data <- mkRegU;
  Reg #(AXI4_Len)   flitIdx <- mkRegU;
  let fsm <- mkRecipeFSM (rSeq(rBlock(
    action
      vPrint(6, $format("mkSlaveSetup - resetting"));
      data <= 'h01234567_89abcdef_02468ace_13579bdf_048c26ae_159d37bf_082a4c6e_193b5d7f_fedcba98_76543210_00000001_11111112_22222223_33333334_fedcba98_76543210;
      flitIdx <= 0;
    endaction
  , action
      AXI4_AWFlit #(t_id, t_addr, t_awuser) awflit = AXI4_AWFlit {
          awaddr: 0
        , awid: 0
        , awsize: fromInteger (valueOf (TDiv #(t_data, 8)))
        , awlen: fromInteger (valueOf (TDiv #(512, t_data)) - 1)
        , awregion: 0
        , awburst: INCR
        , awprot: 0
        , awcache: 0
        , awlock: ?
        , awqos: 0
        , awuser: 0
        };
      slv.aw.put (awflit);
      vPrint(5, $format("mkSlaveSetup - sent ", fshow (awflit)));
    endaction
  , rWhile ( flitIdx <= fromInteger (valueOf (TDiv #(512, t_data)) - 1)
           , rAct(action
      AXI4_WFlit #(t_data, t_wuser) wflit = AXI4_WFlit {
          wdata: truncate (data)
        , wstrb: ~0
        , wlast: True
        , wuser: 0
        };
      slv.w.put (wflit);
      flitIdx <= flitIdx + 1;
      vPrint(5, $format("mkSlaveSetup - sent ", fshow (wflit)));
      vPrint(6, $format("mkSlaveSetup - flitIdx <= ", fshow (flitIdx + 1)));
    endaction))
  , rWhile (!slv.b.canPeek, rAct(
      vPrint(6, $format("mkSlaveSetup - waiting for b flit"))
    ))
  , rAct(action
      let bflit <- get (slv.b);
      vPrint(5, $format("mkSlaveSetup - received ", fshow(bflit)));
    endaction)
  )));
  return fsm;
endmodule

////////////////////////////////////////////////////////////////////////////////
function List #(Tuple3 #(Integer, Integer, Integer))
  genFlitParams (Integer memSize);
  List #(Tuple3 #(Integer, Integer, Integer)) flitParams = Nil;
  for (Integer offset = 0; offset < memSize; offset = offset + 1) begin
    Integer remain = memSize - offset;
    Integer maxSize = 2**log2 (remain);
    for (Integer size = 1; size <= maxSize; size = size * 2) begin
      Integer maxLen = remain / size;
      for (Integer len = 1; len <= maxLen; len = len + 1) begin
        flitParams = cons (tuple3 (offset, size, len), flitParams);
      end
    end
  end
  return flitParams;
endfunction

function Tuple3 #(Bit #(n), AXI4_Size, AXI4_Len)
         toAXI4_Params (Tuple3 #(Integer, Integer, Integer) params);
  match {.offset, .size, .len} = params;
  return tuple3 ( fromInteger (offset)
                , fromInteger (size)
                , fromInteger (len - 1) );
endfunction

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
module mkReadStimuliFSM #( AXI4_Slave #( t_id, t_addr, 512
                                       , t_awuser, t_wuser, t_buser
                                       , t_aruser, t_ruser ) golden
                         , AXI4_Slave #( t_id, t_addr, 512
                                       , t_awuser, t_wuser, t_buser
                                       , t_aruser, t_ruser ) dut ) (RecipeFSM)
  provisos (Add #(__a, 6, t_addr));
  // bookkeepig registers and fifos
  //req side
  let reqParams = map (toAXI4_Params, genFlitParams (valueOf(512)/8));
  let paramSrc <- mkListToSource (reqParams);
  Reg #(Bool) reqDone <- mkRegU;
  // rsp side
  Reg #(AXI4_Len)  rspFlitIdx <- mkRegU;
  Reg #(Bool)         rspDone <- mkRegU;
  Reg #(Bool)         allDone <- mkRegU;
  FIFOF #(Maybe #(Tuple3 #(AXI4_Len, AXI4_Size, Bit #(6)))) ff <- mkFIFOF;
  // helper function
  function Bit #(512) getRelevantData ( Bit #(512) data
                                      , AXI4_Len flitIdx
                                      , AXI4_Size size
                                      , Bit #(6) offset );
    Bit #(6) offsetMask = ~0 << pack (size);
    Bit #(6) alignedOffset = offset & offsetMask;
    Bit #(6) withinSizeOffset = offset & ~offsetMask;
    Bit #(512) mask = ~(~0 << (1 << (pack (size) + 3)));
    if (flitIdx == 0) mask = mask & (~0 << {withinSizeOffset, 3'b000});

    Bit #(9) byteShftAmnt = zeroExtend (alignedOffset) + (1 << pack (size)) * zeroExtend (flitIdx);
    Bit #(512) shiftedData = data >> (byteShftAmnt << 3);
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
      vPrint(4, $format("mkReadStimuliFSM - resetting "));
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
              vPrint (2, $format("mkReadStimuliFSM -  ff.deq, reset rspFlitIdx"));
            end else rspFlitIdx <= rspFlitIdx + 1;
          endaction))
        ))
      , rAct (action
          allDone <= True;
          ff.deq;
          vPrint (2, $format("mkReadStimuliFSM -  ff.first ", fshow(ff.first)));
          vPrint (2, $format("mkReadStimuliFSM -  final dequeue, allDone <= True "));
        endaction)
      )))
    ))
    // all req / rsp pairs have gone through, success
  , die ($format("Success"))
  )));
  return fsm;
endmodule

////////////////////////////////////////////////////////////////////////////////

module testReadWideToNarrow (Empty);
  // golden memory
  AXI4_Slave #(0, 8, 512, 0, 0, 0, 0, 0) goldenMem <- mkAXI4Mem (512, UnInit);
  let setupGolden <- mkSlaveSetup (goldenMem);
  // dut memory
  AXI4_Slave #(0, 8, 64, 0, 0, 0, 0, 0) dutMem <- mkAXI4Mem (512, UnInit);
  NumProxy#(1) one = ?;
  Tuple2 #( AXI4_Slave #(0, 8, 512, 0, 0, 0, 0, 0)
          , AXI4_Master#(0, 8, 64, 0, 0, 0, 0, 0) )
    wide2narrow <- mkAXI4DataWidthShim_WideToNarrow (one, one);
  match {.dutSlave, .dutMaster} = wide2narrow;
  mkConnection (dutMaster, dutMem);
  let setupDUT <- mkSlaveSetup (dutSlave);
  // run test
  let stimuli <- mkReadStimuliFSM (goldenMem, dutSlave);
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







































/*

module testAXI4ReadWidths (Empty);
  Vector#(2, AXI4_Slave#(0, 8, 512, 0, 0, 0, 0, 0)) mems <- replicateM(mkAXI4Mem(512, UnInit));
  let oracleMem = mems[0];
  let testMem = mems[1];
  NumProxy#(1) one = ?;
  /*
  Tuple2#(AXI4_Slave#(0, 8, 64, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 512, 0, 0, 0, 0, 0))
      narrowSw_wideMw <- mkAXI4DataWidthShim_NarrowToWide(one, one);
  match {.narrowSw, .wideMw} = narrowSw_wideMw;
  Tuple2#(AXI4_Slave#(0, 8, 512, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 64, 0, 0, 0, 0, 0))
      wideSn_narrowMn <- mkAXI4DataWidthShim_WideToNarrow(one, one);
  match {.wideSn, .narrowMn} = wideSn_narrowMn;

  Tuple2#(AXI4_Slave#(0, 8, 32, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 64, 0, 0, 0, 0, 0))
      narrowSw_wideMw_inner <- mkAXI4DataWidthShim_NarrowToWide(one, one);
  match {.narrowSw_inner, .wideMw_inner} = narrowSw_wideMw_inner;
  Tuple2#(AXI4_Slave#(0, 8, 64, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 32, 0, 0, 0, 0, 0))
      wideSn_narrowMn_inner <- mkAXI4DataWidthShim_WideToNarrow(one, one);
  match {.wideSn_inner, .narrowMn_inner} = wideSn_narrowMn_inner;

  mkConnection(wideMw, testMem);
  mkConnection(narrowMn, wideSn_inner);
  mkConnection(wideMw_inner, narrowSw);
  mkConnection(narrowMn_inner, narrowSw_inner);
  Tuple2#(AXI4_Slave#(0, 8, 32, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 512, 0, 0, 0, 0, 0))
      narrowSw_wideMw <- mkAXI4DataWidthShim_NarrowToWide(one, one);
  match {.narrowSw, .wideMw} = narrowSw_wideMw;
  Tuple2#(AXI4_Slave#(0, 8, 512, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 32, 0, 0, 0, 0, 0))
      wideSn_narrowMn <- mkAXI4DataWidthShim_WideToNarrow(one, one);
  match {.wideSn, .narrowMn} = wideSn_narrowMn;
  mkConnection(wideMw, testMem);
  mkConnection(narrowMn, narrowSw);

  Reg#(Bit#(8)) size <- mkReg(1);
  Reg#(Bit#(6)) offset <- mkReg(0);
  FIFO#(Maybe#(Tuple2#(Bit#(8), Bit#(6)))) trackFF <- mkFIFO;
  Reg#(Bool) init <- mkReg(False);
  Reg#(Bool) setupSent <- mkReg(False);

  rule setup(!setupSent);
      let awflit = AXI4_AWFlit { awaddr : 0
                               , awid : 0
                               , awsize : 64
                               , awlen: 0
                               , awregion: 0
                               , awburst: INCR
                               , awprot: 0
                               , awcache: 0
                               , awlock: ?
                               , awqos: 0
                               , awuser : 0};
      let wflit = AXI4_WFlit { wdata: 'h01234567_89abcdef_02468ace_13579bdf_048c26ae_159d37bf_082a4c6e_193b5d7f_fedcba98_76543210_00000001_11111112_22222223_33333334_44444445_55555556
                             , wstrb: ~0
                             , wlast: True
                             , wuser: 0};
      oracleMem.aw.put(awflit);
      wideSn.aw.put(awflit);
      oracleMem.w.put(wflit);
      wideSn.w.put(wflit);
      setupSent <= True;
  endrule

  rule drainB;
      oracleMem.b.drop;
      wideSn.b.drop;
      init <= True;
  endrule

  rule emitReqs(init);
      Bit#(6) nextOffset = truncate(zeroExtend(offset) + size);
      Bit#(8) nextSize = nextOffset == 0 ? size << 1 : size;
      if (nextSize <= 64) begin
          let arflit = AXI4_ARFlit { araddr: zeroExtend(offset)
                                   , arid: 0
                                   , aruser: 0
                                   , arlen: 0
                                   , arburst: INCR
                                   , arcache: 0
                                   , arlock: ?
                                   , arregion: 0
                                   , arqos: 0
                                   , arprot: 0
                                   , arsize: toAXI4_Size(zeroExtend(size)).Valid};
          wideSn.ar.put(arflit);
          oracleMem.ar.put(arflit);
          trackFF.enq(Valid(tuple2(size, offset)));
          size <= nextSize;
          offset <= nextOffset;
      end else begin
          trackFF.enq(Invalid);
      end
  endrule

  rule compareRsps(isValid(trackFF.first));
      match {.size, .offset} = trackFF.first.Valid;
      let testR <- get(wideSn.r);
      let oracleR <- get(oracleMem.r);
      let testData = getRelevantData(oracleR.rdata, offset, size);
      let oracleData = getRelevantData(testR.rdata, offset, size);
      $display("%0t - ", $time, "reqsize: ", fshow(size), ", reqoffset: ", fshow(offset), ", reqlen: ", fshow(len)
           , "\noracle: ", fshow(oracleR)
           , "\ntest  : ", fshow(testR)
           , "\noracle data: ", fshow(oracleData)
           , "\ntest data  : ", fshow(testData));
      if(testData != oracleData) begin
          $display("Fail");
          $finish;
      end
      if (oracleR.rlast) begin
        $display("------------------------------------------------");
        trackFF.deq();
      end
  endrule

  rule succeed(!isValid(trackFF.first));
      $display("success");
      $finish;
  endrule
endmodule

module testAXI4WriteWidths (Empty);
  Vector#(2, AXI4_Slave#(0, 8, 512, 0, 0, 0, 0, 0)) mems <- replicateM(mkAXI4Mem(512, UnInit));
  let oracleMem = mems[0];
  let testMem = mems[1];
  NumProxy#(1) one = ?;
  Tuple2#(AXI4_Slave#(0, 8, 64, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 512, 0, 0, 0, 0, 0))
      narrowSw_wideMw <- mkAXI4DataWidthShim_NarrowToWide(one, one);
  match {.narrowSw, .wideMw} = narrowSw_wideMw;
  Tuple2#(AXI4_Slave#(0, 8, 512, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 64, 0, 0, 0, 0, 0))
      wideSn_narrowMn <- mkAXI4DataWidthShim_WideToNarrow(one, one);
  match {.wideSn, .narrowMn} = wideSn_narrowMn;

  Tuple2#(AXI4_Slave#(0, 8, 32, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 64, 0, 0, 0, 0, 0))
      narrowSw_wideMw_inner <- mkAXI4DataWidthShim_NarrowToWide(one, one);
  match {.narrowSw_inner, .wideMw_inner} = narrowSw_wideMw_inner;
  Tuple2#(AXI4_Slave#(0, 8, 64, 0, 0, 0, 0, 0), AXI4_Master#(0, 8, 32, 0, 0, 0, 0, 0))
      wideSn_narrowMn_inner <- mkAXI4DataWidthShim_WideToNarrow(one, one);
  match {.wideSn_inner, .narrowMn_inner} = wideSn_narrowMn_inner;

  mkConnection(wideMw, testMem);
  mkConnection(narrowMn, wideSn_inner);
  mkConnection(wideMw_inner, narrowSw);
  mkConnection(narrowMn_inner, narrowSw_inner);

  Reg#(Bit#(8)) size <- mkReg(1);
  Reg#(Bit#(6)) offset <- mkReg(0);
  FIFOF#(Maybe#(Tuple2#(Bit#(8), Bit#(6)))) trackFF <- mkFIFOF;
  Reg#(Bool) init <- mkReg(False);
  Reg#(Bool) setupSent <- mkReg(False);
  Get #(Bit #(512)) rng <- mkRNG (~0);

  rule setup(!setupSent);
      let awflit = AXI4_AWFlit { awaddr : 0
                               , awid : 0
                               , awsize : 64
                               , awlen: 0
                               , awregion: 0
                               , awburst: INCR
                               , awprot: 0
                               , awcache: 0
                               , awlock: ?
                               , awqos: 0
                               , awuser : 0};
      let wflit = AXI4_WFlit { wdata: 'h01234567_89abcdef_02468ace_13579bdf_048c26ae_159d37bf_082a4c6e_193b5d7f_fedcba98_76543210_00000001_11111112_22222223_33333334_44444445_55555556
                             , wstrb: ~0
                             , wlast: True
                             , wuser: 0};
      oracleMem.aw.put(awflit);
      wideSn.aw.put(awflit);
      oracleMem.w.put(wflit);
      wideSn.w.put(wflit);
      setupSent <= True;
  endrule

  rule drainB (!init);
      oracleMem.b.drop;
      wideSn.b.drop;
      init <= True;
  endrule

  rule emitReqs(init && !trackFF.notEmpty);
      Bit#(6) nextOffset = truncate(zeroExtend(offset) + size);
      Bit#(8) nextSize = nextOffset == 0 ? size << 1 : size;
      Bit#(512) val <- rng.get;
      if (nextSize <= 64) begin
          AXI4_AWFlit #(0, 8, 0) awflit = AXI4_AWFlit { awaddr: zeroExtend(offset)
                                   , awid: 0
                                   , awuser: 0
                                   , awlen: 0
                                   , awburst: INCR
                                   , awcache: 0
                                   , awlock: ?
                                   , awregion: 0
                                   , awqos: 0
                                   , awprot: 0
                                   , awsize: toAXI4_Size(zeroExtend(size)).Valid};
          AXI4_WFlit #(512, 0) wflit = AXI4_WFlit { wdata: val
                                 , wstrb: ~(~0<<size)<<offset
                                 , wlast: True
                                 , wuser: 0};
          wideSn.aw.put(awflit);
          wideSn.w.put(wflit);
          oracleMem.aw.put(awflit);
          oracleMem.w.put(wflit);
          $display ("reqAW: ", fshow (awflit));
          $display ("reqW: ", fshow (wflit));
          trackFF.enq(Valid(tuple2(size, offset)));
          size <= nextSize;
          offset <= nextOffset;
      end else begin
          trackFF.enq(Invalid);
      end
  endrule

  rule drainBB (init);
    oracleMem.b.drop;
    wideSn.b.drop;
    let arflit = AXI4_ARFlit { araddr: 0
                             , arid: 0
                             , aruser: 0
                             , arlen: 0
                             , arburst: INCR
                             , arcache: 0
                             , arlock: ?
                             , arregion: 0
                             , arqos: 0
                             , arprot: 0
                             , arsize: 64 };
    oracleMem.ar.put(arflit);
    wideSn.ar.put(arflit);
  endrule

  rule compareRsps(isValid(trackFF.first));
      match {.size, .offset} = trackFF.first.Valid;
      let testR <- get(wideSn.r);
      let oracleR <- get(oracleMem.r);
      let testData = oracleR.rdata;
      let oracleData = testR.rdata;
      $display("reqsize: ", fshow(size), ", reqoffset: ", fshow(offset)
           , "\noracle: ", fshow(oracleR)
           , "\ntest  : ", fshow(testR) );
      if(testData != oracleData) begin
          $display("Fail");
          $finish;
      end
      trackFF.deq();
  endrule

  rule succeed(!isValid(trackFF.first));
      $display("success");
      $finish;
  endrule
endmodule

*/

endpackage
