/*-
 * Copyright (c) 2018-2022 Alexandre Joannou
 * All rights reserved.
 *
 * This hardware design was developed by the University of Cambridge Computer
 * Laboratory (Department of Computer Science and Technology) under EPSRC award
 * EP/S030867/1 ("SIPP"); and by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
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

package AXI4_Utils_Width_Shim;

import Vector :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Assert :: *; // must build with the "-check-assert" flag to enable

import BlueBasics :: *;

import AXI4_Types :: *;
import AXI4_Common_Types :: *;

// exported utilities

export mkAXI4DataWidthShim_WideToNarrow;
export mkAXI4DataWidthShim_NarrowToWide;

// debug helpers

Integer verbosity_level = 0;
function Action vPrint (Integer lvl, Fmt fmt) =
  action if (verbosity_level >= lvl) $display ("%0t - ", $time, fmt); endaction;

//////////////////////
// exposed wrappers //
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// AXI4 wide to narrow data shim
////////////////////////////////////////////////////////////////////////////////

module mkAXI4DataWidthShim_WideToNarrow
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth )
  // returned interface
  (Tuple2 #( AXI4_Slave  #( id_, addr_, data_X
                          , awuser_, wuser_, buser_, aruser_, ruser_ )
           , AXI4_Master #( id_, addr_, data_Y
                          , awuser_, wuser_, buser_, aruser_, ruser_ )))
  provisos (Add#(_a, data_Y, data_X));
  match {.aw_X, .w_X, .b_X, .aw_Y, .w_Y, .b_Y}
    <- mkAXI4WritesWideToNarrow (proxyBuffInDepth, proxyBuffOutDepth);
  match {.ar_X, .r_X, .ar_Y, .r_Y}
    <- mkAXI4ReadsWideToNarrow (proxyBuffInDepth, proxyBuffOutDepth);
  return tuple2 (
    interface AXI4_Slave;
      interface aw = aw_X;
      interface  w = w_X;
      interface  b = b_X;
      interface ar = ar_X;
      interface  r = r_X;
    endinterface
  , interface AXI4_Master;
      interface aw = aw_Y;
      interface  w = w_Y;
      interface  b = b_Y;
      interface ar = ar_Y;
      interface  r = r_Y;
    endinterface );
endmodule

// AXI4 narrow to wide data shim
////////////////////////////////////////////////////////////////////////////////

module mkAXI4DataWidthShim_NarrowToWide
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth )
  // returned interface
  (Tuple2 #( AXI4_Slave  #( id_, addr_, data_X
                          , awuser_, wuser_, buser_, aruser_, ruser_ )
           , AXI4_Master #( id_, addr_, data_Y
                          , awuser_, wuser_, buser_, aruser_, ruser_ )))
  provisos ( Add #(_a, data_X, data_Y)
           , Add #(_b, TDiv#(data_X, 8), TDiv#(data_Y, 8))
           , Mul #(TDiv#(data_Y, 8), 8, data_Y)
           , Add #(_c, TLog#(TDiv#(data_X, 8)), TLog#(TDiv#(data_Y, 8)))
           , Add #(_d, TLog#(TDiv#(data_X, 8)), TLog#(data_X))
           , Add #(_e, TLog#(TDiv#(data_Y, 8)), TLog#(data_Y))
           , Add #(_f, TLog#(TDiv#(data_Y, 8)), 16)
           , Add #(_g, TLog#(TDiv#(data_Y, 8)), addr_)
           , Add #(_h, TLog#(data_X), TLog#(data_Y)) );
  match {.aw_X, .w_X, .b_X, .aw_Y, .w_Y, .b_Y}
    <- mkAXI4WritesNarrowToWide (proxyBuffInDepth, proxyBuffOutDepth);
  match {.ar_X, .r_X, .ar_Y, .r_Y}
    <- mkAXI4ReadsNarrowToWide (proxyBuffInDepth, proxyBuffOutDepth);
  return tuple2 (
    interface AXI4_Slave;
      interface aw = aw_X;
      interface  w = w_X;
      interface  b = b_X;
      interface ar = ar_X;
      interface  r = r_X;
    endinterface
  , interface AXI4_Master;
      interface aw = aw_Y;
      interface  w = w_Y;
      interface  b = b_Y;
      interface ar = ar_Y;
      interface  r = r_Y;
    endinterface );
endmodule

/////////////////////
// implementations //
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Convert wide writes to narrow writes
////////////////////////////////////////////////////////////////////////////////

module mkAXI4WritesWideToNarrow
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth)
  // returned interface
  (Tuple6 #( Sink #(AXI4_AWFlit #(id_, addr_, awuser_))
           , Sink #(AXI4_WFlit #(data_X, wuser_))
           , Source #(AXI4_BFlit #(id_, buser_))
           , Source #(AXI4_AWFlit #(id_, addr_, awuser_))
           , Source #(AXI4_WFlit #(data_Y, wuser_))
           , Sink #(AXI4_BFlit #(id_, buser_)) ))
  provisos (Add #(_, data_Y, data_X));

  // local declarations
  //////////////////////////////////////////////////////////////////////////////
  // interfaces //
  ////////////////
  // Address request channel, altered to account for burst size changes
  FIFOF #(AXI4_AWFlit #(id_, addr_, awuser_)) awffIn <- mkFIFOF;
  FIFOF #(AXI4_AWFlit #(id_, addr_, awuser_)) awffOut <- mkFIFOF;
  // Data request channel
  FIFOF #(AXI4_WFlit #(data_X, wuser_))
    wffIn <- mkSizedFIFOF (valueOf (buffInDepth));
  FIFOF #(AXI4_WFlit #(data_Y, wuser_))
    wffOut <- mkSizedFIFOF (valueOf (buffOutDepth));
  // Response channel, single flit, passed straight through
  let bff <- mkFIFOF;
  ////////////
  // others //
  ////////////
  // local communication
  //TODO let reqff <- mkBypassFIFOF;
  FIFOF #(Bit #(0)) reqff <- mkBypassFIFOF;

  // handle address channel
  //////////////////////////////////////////////////////////////////////////////
  rule aw_send;
    vPrint (1, $format ("%m.mkAXI4WritesWideToNarrow.aw_send"));
    // read and consume the incoming address request
    let awflitIn <- get (awffIn);
    vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow.aw_send, "
                       , "awflitIn ", fshow (awflitIn) ));
    // derive the new outgoing address request
    AXI4_AWFlit #(id_, addr_, awuser_) awflitOut = error ("TODO");
    // send the outgoing address request
    awffOut.enq (awflitOut);
    vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow.aw_send, "
                       , "awflitOut ", fshow (awflitOut) ));
    // pass local information to the data channel handling rule
    Bit #(0) reqffpayload = error ("TODO");
    reqff.enq (reqffpayload);
    vPrint (3, $format ( "%m.mkAXI4WritesWideToNarrow.aw_send, "
                       , "reqffpayload ", fshow (reqffpayload) ));
  endrule

  // handle data channel
  //////////////////////////////////////////////////////////////////////////////
  rule w_send;
    vPrint (1, $format ("%m.mkAXI4WritesWideToNarrow.w_send"));
    // read current local information
    let reqffpayload = reqff.first;
    vPrint (3, $format ( "%m.mkAXI4WritesWideToNarrow.w_send, "
                       , "reqffpayload ", fshow (reqffpayload) ));
    // read current incoming data flit
    let wflitIn = wffIn.first;
    vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow.w_send, "
                       , "wflitIn ", fshow (wflitIn) ));
    // derive the new outgoing data flit
    AXI4_WFlit #(data_Y, wuser_) wflitOut = error ("TODO");
    // send the outgoing data flit
    wffOut.enq (wflitOut);
    vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow.w_send, "
                       , "wflitOut ", fshow (wflitOut) ));
    // when the whole request is handled, consume local information
    let requestHandled = error ("TODO");
    if (requestHandled) begin
      reqff.deq;
      vPrint (3, $format ( "%m.mkAXI4WritesWideToNarrow.w_send, "
                         , "whole request handled" ));
    end
    // when the incoming flit is fully utilized (and on end of request),
    // consume it
    let flitUtilized = error ("TODO");
    if (requestHandled || flitUtilized) begin
      wffIn.deq;
      vPrint (3, $format ( "%m.mkAXI4WritesWideToNarrow.w_send, "
                         , "wflitIn fully utilized, consuming it" ));
    end
  endrule

  // return channels as interface
  //////////////////////////////////////////////////////////////////////////////
  return tuple6 ( toSink (awffIn), toSink (wffIn), toSource (bff)
                , toSource (awffOut), toSource(wffOut), toSink (bff) );

endmodule

// Convert wide reads to narrow reads
////////////////////////////////////////////////////////////////////////////////

module mkAXI4ReadsWideToNarrow
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth)
  // returned interface
  (Tuple4 #( Sink #(AXI4_ARFlit #(id_, addr_, aruser_))
           , Source #(AXI4_RFlit #(id_, data_X, ruser_))
           , Source #(AXI4_ARFlit #(id_, addr_, aruser_))
           , Sink #(AXI4_RFlit #(id_, data_Y, ruser_)) ))
  provisos (Add #(_, data_Y, data_X));
  let ifcs = error ("TODO");
  return ifcs;
endmodule

// Convert narrow writes to wide writes
////////////////////////////////////////////////////////////////////////////////

// XXX TODO: currently supports awburst == INCR
//           undefined behaviour for WRAP and FIXED (in practice same as INCR)

module mkAXI4WritesNarrowToWide
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth)
  // returned interface
  (Tuple6 #( Sink #(AXI4_AWFlit #(id_, addr_, awuser_))
           , Sink #(AXI4_WFlit #(data_X, wuser_))
           , Source #(AXI4_BFlit #(id_, buser_))
           , Source #(AXI4_AWFlit #(id_, addr_, awuser_))
           , Source #(AXI4_WFlit #(data_Y, wuser_))
           , Sink #(AXI4_BFlit #(id_, buser_)) ))
  provisos ( NumAlias #(nBytes_t, TAdd #( SizeOf #(AXI4_Len)
                                        , TExp #(SizeOf #(AXI4_Size)) ))
           , NumAlias #(in_bit_idx_t, TLog #(data_X))
           , NumAlias #(in_byte_idx_t, TLog #(TDiv #(data_X, 8)))
           , NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_bit_idx_t, TLog #(data_Y))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Add #(_a, data_X, data_Y)
           , Add #(_b, TDiv#(data_X, 8), TDiv#(data_Y, 8))
           , Mul #(TDiv#(data_Y, 8), 8, data_Y)
           , Add #(_c, out_byte_idx_t, nBytes_t)
           , Add #(_d, in_byte_idx_t, in_bit_idx_t)
           , Add #(_e, out_byte_idx_t, out_bit_idx_t)
           , Add #(_f, in_byte_idx_t, out_byte_idx_t)
           , Add #(_r, in_bit_idx_t, out_bit_idx_t)
           , Add #(_h, out_byte_idx_t, addr_) );

  // Address request channel, altered to account for burst size changes
  FIFOF #(AXI4_AWFlit #(id_, addr_, awuser_)) awffIn <- mkFIFOF;
  FIFOF #(AXI4_AWFlit #(id_, addr_, awuser_)) awffOut <- mkFIFOF;

  // Data request channel
  FIFOF #(AXI4_WFlit #(data_X, wuser_))
    wffIn <- mkSizedFIFOF (valueOf (buffInDepth));
  FIFOF #(AXI4_WFlit #(data_Y, wuser_))
    wffOut <- mkSizedFIFOF (valueOf (buffOutDepth));

  // Response channel, single flit, passed straight through
  let bff <- mkFIFOF;

  // local communication
  let reqff <- mkBypassFIFOF;

  // handle address flits
  rule aw_send;
    vPrint (1, $format ("%m.mkAXI4WritesNarrowToWide.aw_send"));
    AXI4_AWFlit #(id_, addr_, awuser_) awflitIn <- get (awffIn);
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.aw_send, "
                       , "awflitIn ", fshow (awflitIn) ));
    Bit #(nBytes_t) nBytes =
      (zeroExtend (awflitIn.awlen) + 1) << pack (awflitIn.awsize);
    AXI4_Len awlenOut =
      truncate (nBytes >> ((valueOf (TDiv #(data_Y, 8))) - 1));
    AXI4_Size awsizeOut = awflitIn.awsize;
    Bit #(TExp #(SizeOf #(AXI4_Size))) busBytes =
      fromInteger (valueOf (out_byte_t));
    if (nBytes >= zeroExtend (busBytes)) begin
      case (toAXI4_Size (busBytes)) matches
        tagged Valid .x: awsizeOut = x;
        default : begin
          $display ("error: impossible awsize encountered");
          $finish;
        end
      endcase
    end
    let awflitOut = AXI4_AWFlit { awid: awflitIn.awid
                                , awaddr: awflitIn.awaddr
                                , awlen: truncate (awlenOut)
                                , awsize: awsizeOut
                                , awburst: awflitIn.awburst
                                , awlock: awflitIn.awlock
                                , awcache: awflitIn.awcache
                                , awprot: awflitIn.awprot
                                , awqos: awflitIn.awqos
                                , awregion: awflitIn.awregion
                                , awuser: awflitIn.awuser };
    awffOut.enq (awflitOut);
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.aw_send, "
                       , "awflitOut ", fshow (awflitOut) ));
    let reqffpayload = tuple6 ( nBytes, awflitIn.awaddr
                              , awflitIn.awsize, awflitIn.awlen
                              , awsizeOut, awlenOut );
    reqff.enq (reqffpayload);
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.aw_send, "
                       , "reqffpayload ", fshow (reqffpayload) ));
  endrule

  // handle data flits
  Reg #(Bit #(nBytes_t)) cnt <- mkReg (0);
  Reg #(Bit #(TDiv #(data_Y, 8))) strb <- mkReg (0);
  Reg #(Bit #(data_Y)) data <- mkRegU;
  rule w_accumulate_send;
    vPrint (1, $format ("%m.mkAXI4WritesNarrowToWide.w_accumulate_send"));
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "reqff.first ", fshow (reqff.first) ));
    match {.nBytes, .addr, .awsizeIn, .awlenIn, .awsizeOut, .awlenOut} =
      reqff.first;
    let wflitIn <- get (wffIn);
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "wflitIn ", fshow (wflitIn) ));

    Bit #(out_byte_idx_t) width = 1 << pack (awsizeIn);
    Bit #(out_byte_idx_t) loOut = truncate (addr) + truncate (cnt);
    Bit #(in_byte_idx_t) loIn = truncate (loOut);
    Bit #(out_bit_idx_t) loOutBit = zeroExtend (loOut) << 3;
    Bit #(in_bit_idx_t) loInBit = truncate (loOutBit);

    // accumulate the data and book-keep
    Bit #(nBytes_t) newCnt = cnt + zeroExtend (width);
    //tmpStrb[hiOut:loOut] = wflitIn.wstrb[hiIn:loIn];
    //tmpData[hiOutBit:loOutBit] = wflitIn.wdata[hiInBit:loInBit];
    Bit #(TDiv #(data_Y, 8)) msk = ~(~0 << width) << loOut;
    Bit #(TDiv #(data_X, 8)) tmpStrbIn = wflitIn.wstrb >> loIn;
    Bit #(TDiv #(data_Y, 8)) tmpStrbOut = zeroExtend (tmpStrbIn) << loOut;
    Bit #(TDiv #(data_Y, 8)) newStrb = mergeWithMask (msk, strb, tmpStrbOut);
    Bit #(data_X) tmpDataIn = wflitIn.wdata >> loInBit;
    Bit #(data_Y) tmpDataOut = zeroExtend (tmpDataIn) << loOutBit;
    Bit #(data_Y) newData = mergeWithBE (msk, data, tmpDataOut);

    // did we reach the last flit
    Bool isLast = newCnt == nBytes;
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "newCnt ", fshow (newCnt) ));
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "nBytes ", fshow (nBytes) ));

    // full flit ready
    Bit #(out_byte_idx_t) cntOffset = truncate (newCnt);
    if (isLast || cntOffset == 0) begin
      let wflitOut =  AXI4_WFlit { wdata: newData
                                 , wstrb: newStrb
                                 , wlast: isLast
                                   // XXX better thing to do here?
                                 , wuser: wflitIn.wuser
                                 };
      wffOut.enq (wflitOut);
      vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                         , "wflitOut ", fshow (wflitOut) ));
      newData = 0;
      newStrb = 0;
    end

    // finished the burst
    if (isLast) begin
      vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                         , "burst finished"));

      // assertions
      // must build with the "-check-assert" flag to enable
      dynamicAssert (wflitIn.wlast, "should line up with last w flit");

      reqff.deq;
      newCnt = 0;

    end

    // state update
    cnt <= newCnt;
    data <= newData;
    strb <= newStrb;
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "cnt ", fshow (cnt) ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "newCnt ", fshow (newCnt) ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "data ", fshow (data) ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "newData ", fshow (newData) ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "strb ", fshow (strb) ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "newStrb ", fshow (newStrb) ));

  endrule

  return tuple6 ( toSink (awffIn), toSink (wffIn), toSource (bff)
                , toSource (awffOut), toSource(wffOut), toSink (bff) );

endmodule

// Convert narrow reads to wide reads
////////////////////////////////////////////////////////////////////////////////

// XXX TODO: currently supports awburst == INCR
//           undefined behaviour for WRAP and FIXED (in practice same as INCR)

module mkAXI4ReadsNarrowToWide
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth)
  // returned interface
  (Tuple4 #( Sink #(AXI4_ARFlit #(id_, addr_, aruser_))
           , Source #(AXI4_RFlit #(id_, data_X, ruser_))
           , Source #(AXI4_ARFlit #(id_, addr_, aruser_))
           , Sink #(AXI4_RFlit #(id_, data_Y, ruser_)) ))
  provisos ( NumAlias #(nBytes_t, TAdd #( SizeOf #(AXI4_Len)
                                        , TExp #(SizeOf #(AXI4_Size)) ))
           , NumAlias #(in_bit_idx_t, TLog #(data_X))
           , NumAlias #(in_byte_idx_t, TLog #(TDiv #(data_X, 8)))
           , NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_bit_idx_t, TLog #(data_Y))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Alias #(local_info, Tuple4 #( Bit #(nBytes_t)
                                         , Bit #(addr_)
                                         , AXI4_Size
                                         , AXI4_Len ))
           , Add #(_a, data_X, data_Y)
           , Add #(_b, out_byte_idx_t, nBytes_t)
           , Add #(_c, in_byte_idx_t, in_bit_idx_t)
           , Add #(_d, out_byte_idx_t, out_bit_idx_t)
           , Add #(_e, in_byte_idx_t, out_byte_idx_t)
           , Add #(_f, in_bit_idx_t, out_bit_idx_t)
           , Add #(_g, out_byte_idx_t, addr_) );

  // Request channel, single flit
  FIFOF #(AXI4_ARFlit #(id_, addr_, aruser_)) arffIn <- mkFIFOF;
  FIFOF #(AXI4_ARFlit #(id_, addr_, aruser_)) arffOut <- mkFIFOF;

  // Data response channel
  FIFOF #(AXI4_RFlit #(id_, data_X, ruser_))
    rffIn <- mkSizedFIFOF (valueOf (buffInDepth));
  FIFOF #(AXI4_RFlit #(id_, data_Y, ruser_))
    rffOut <- mkSizedFIFOF (valueOf (buffOutDepth));

  // local state to remember addresses
  Vector #(TExp #(id_), FIFOF #(local_info)) addrff <- replicateM (mkFIFOF);

  rule ar_send;
    vPrint (1, $format ("%m.mkAXI4ReadsNarrowToWide.ar_send"));
    let arflitIn <- get (arffIn);
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.ar_send, arflitIn "
                       , fshow (arflitIn) ));
    Bit #(nBytes_t) nBytes =
      (zeroExtend (arflitIn.arlen) + 1) << pack (arflitIn.arsize);
    AXI4_Len arlenOut =
      truncate (nBytes >> ((valueOf (TDiv #(data_Y, 8))) - 1));
    AXI4_Size arsizeOut = arflitIn.arsize;
    Bit #(TExp #(SizeOf #(AXI4_Size))) busBytes =
      fromInteger (valueOf (out_byte_t));
    if (nBytes >= zeroExtend (busBytes)) begin
      case (toAXI4_Size (busBytes)) matches
        tagged Valid .x: arsizeOut = x;
        default : begin
          vPrint (1, $format ("error: impossible arsize encountered"));
          $finish;
        end
      endcase
    end
    let arflitOut = AXI4_ARFlit { arid: arflitIn.arid
                                , araddr: arflitIn.araddr
                                , arlen: arlenOut
                                , arsize: arsizeOut
                                , arburst: arflitIn.arburst
                                , arlock: arflitIn.arlock
                                , arcache: arflitIn.arcache
                                , arprot: arflitIn.arprot
                                , arqos: arflitIn.arqos
                                , arregion: arflitIn.arregion
                                , aruser: arflitIn.aruser };
    arffOut.enq (arflitOut);
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.ar_send, arflitOut "
                       , fshow (arflitOut) ));
    let addrffpayload = tuple4 ( nBytes
                               , arflitIn.araddr
                               , arflitIn.arsize
                               , arflitIn.arlen );
    addrff[arflitIn.arid].enq (addrffpayload);
    vPrint (3, $format ( "%m.mkAXI4ReadsNarrowToWide.ar_send, "
                       , "addrffpayload[%0d] "
                       , arflitIn.arid, fshow (addrffpayload) ));
  endrule


  // handle data flits
  Reg #(Bit #(nBytes_t)) cnt <- mkReg (0);
  let rflitOut = rffOut.first;
  let addrffpayload = addrff[rflitOut.rid].first;
  match {.nBytes, .addr, .arsize, .arlen} = addrffpayload;
  rule r_accumulate_send (addrff[rflitOut.rid].notEmpty);
    vPrint (1, $format ("%m.mkAXI4ReadsNarrowToWide.r_accumulate_send"));
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                       , "rflitOut ", fshow (rflitOut) ));
    vPrint (3, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                       , "addrffpayload[%0d] "
                       , rflitOut.rid, fshow (addrffpayload) ));

    Bit #(out_byte_idx_t) width = 1 << pack (arsize);
    Bit #(out_byte_idx_t) loOut = truncate (addr) + truncate (cnt);
    Bit #(out_bit_idx_t) loOutBit = zeroExtend (loOut) << 3;
    Bit #(in_bit_idx_t) loInBit = truncate (loOutBit);

    // accumulate the data and book-keep
    Bit #(nBytes_t) newCnt = cnt + zeroExtend (width);
    //rspData[hiInBit:loInBit] = rflitOut.rdata[hiOutBit:loOutBit];
    Bit #(data_Y) rspDataOut = rflitOut.rdata >> loOutBit;
    Bit #(data_X) rspDataIn = truncate (rspDataOut << loInBit);

    // did we reach the last flit
    Bool isLast = newCnt == nBytes;

    // push a response
    let rflitIn = AXI4_RFlit { rid: rflitOut.rid
                             , rdata: rspDataIn
                             , rresp: rflitOut.rresp
                             , rlast: isLast
                             , ruser: rflitOut.ruser };
    rffIn.enq (rflitIn);
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                       , "rflitIn ", fshow (rflitIn) ));

    // will we consume from the returning r flits fifo?
    Bool rffOutConsume = False;

    // finished the burst
    if (isLast) begin
      vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                         , "last flit" ));

      // assertions
      // must build with the "-check-assert" flag to enable
      dynamicAssert (rflitOut.rlast, "should line up with last r flit");

      addrff[rflitOut.rid].deq;
      newCnt = 0;
      rffOutConsume = True;

    end

    // full flit consumed
    Bit #(out_byte_idx_t) cntOffset = truncate (newCnt);
    rffOutConsume = rffOutConsume || (cntOffset == 0);
    if (rffOutConsume) begin
      rffOut.deq;
      vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                         , "full Out flit consumed" ));
    end

    // state update
    cnt <= newCnt;

  endrule

  return tuple4 ( toSink (arffIn), toSource (rffIn)
                , toSource (arffOut), toSink (rffOut) );

endmodule

endpackage
