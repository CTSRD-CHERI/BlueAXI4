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
import Real :: *; // just to test if an Integer is a power of 2

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
  provisos ( NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Add #(_a, data_Y, data_X)
           , Add #(_b, out_byte_idx_t, MaxBytesSz)
           , Add #(_c, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           );
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
  provisos ( NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Add #(_a, data_X, data_Y)
           , Add #(_b, TDiv#(data_X, 8), TDiv#(data_Y, 8))
           , Mul #(TDiv#(data_Y, 8), 8, data_Y)
           , Add #(_c, TLog#(TDiv#(data_X, 8)), TLog#(TDiv#(data_Y, 8)))
           , Add #(_d, TLog#(TDiv#(data_X, 8)), TLog#(data_X))
           , Add #(_e, TLog#(TDiv#(data_Y, 8)), TLog#(data_Y))
           , Add #(_f, TLog#(TDiv#(data_Y, 8)), 16)
           , Add #(_g, TLog#(TDiv#(data_Y, 8)), addr_)
           , Add #(_h, TLog#(data_X), TLog#(data_Y))
           , Add #(_i, out_byte_idx_t, MaxBytesSz)
           , Add #(_j, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           );
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

// common helpers
////////////////////////////////////////////////////////////////////////////////

// numeric type:
// number of bits required to hold the max number of bytes in a transfer
typedef TAdd #( // AXI4 len width + 1 because of representation is -1
                TAdd #(SizeOf #(AXI4_Len), 1)
                // (2^(AXI4 size width))-1 maximum shift amount
              , TSub #(TExp #(SizeOf #(AXI4_Size)), 1) ) MaxBytesSz;

// check if a known static integer is a power of 2
function Bool isPowerOf2 (Integer x);
  Real realX = fromInteger (x);
  Real log2X = log2 (realX);
  return log2X == fromInteger (ceil (log2X));
endfunction

// derive new AXI4 len and size (and total bytes accessed)
typedef Tuple3 #(
           Bit #(MaxBytesSz) // total number of bytes of the access
         , AXI4_Len               // AXI4 len for the desired new bus width
         , AXI4_Size              // AXI4 size for the desired new bus width
         ) AccessParams;
function ActionValue #(AccessParams)
  deriveAccessParams ( NumProxy #(newBusByteW) proxy // new bus width in bytes
                     , AXI4_Len lenIn // original AXI4 len
                     , AXI4_Size sizeIn // original AXI4 size
                     )
  provisos ( NumAlias #(busOffset_t, TLog #(newBusByteW))
           , NumAlias #(flitIdx_t, TSub #(MaxBytesSz, busOffset_t))
           , Add #(_a, busOffset_t, MaxBytesSz)
           , Add #(_b, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, busOffset_t))
           ) = actionvalue

  // assert that the bus width is a power of 2
  if (!isPowerOf2 (valueOf (newBusByteW))) begin
    $display ("desired bus width should be a power of 2");
    $finish;
  end

  // compute number of bytes in the access and derived values
  ///////////////////////////////////////////////////////////
  Bit #(MaxBytesSz) nBytes = (zeroExtend (lenIn) + 1) << pack (sizeIn);
  Bit #(busOffset_t) overflow = truncate (nBytes);
  Bit #(flitIdx_t) nFlits = truncateLSB (nBytes);
  if (overflow == 0 && nFlits == 0) begin
      $display ("error: encountered AXI4 transfer with 0 flits");
      $finish;
  end

  // compute the target access length and check that it is less the maximum
  // AXI4 len of 256
  /////////////////////////////////////////////////////////////////////////
  if (overflow == 0 && nFlits > 256) begin
      $display ("error: too long AXI4 transfer (>256 flits) encountered");
      $finish;
  end
  AXI4_Len lenOut = truncate (nFlits - 1);

  // compute the target access size and check that it is less the maximum
  // AXI4 len of 128 bytes (3'b111)
  ///////////////////////////////////////////////////////////////////////
  AXI4_Size sizeOut = sizeIn;
  if (nBytes >= fromInteger (valueOf (newBusByteW)))
    case (toAXI4_Size (fromInteger (valueOf (newBusByteW)))) matches
      tagged Valid .x: sizeOut = x;
      default: begin
        $display ("error: impossible AXI4 size encountered");
        $finish;
      end
    endcase

  // if there was an overflow, fall back to original access prameters
  // (this can only happen when going from a narrow to a wide bus)
  ///////////////////////////////////////////////////////////////////
  if (overflow != 0) begin
    lenOut = lenIn;
    sizeOut = sizeIn;
  end

  // return results
  /////////////////
  return tuple3 (nBytes, lenOut, sizeOut);

endactionvalue;

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
  provisos ( NumAlias #(in_bit_idx_t, TLog #(data_X))
           , NumAlias #(in_byte_idx_t, TLog #(TDiv #(data_X, 8)))
           , NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_bit_idx_t, TLog #(data_Y))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Add #(_a, data_Y, data_X)
           , Add #(_b, out_byte_idx_t, MaxBytesSz)
           , Add #(_c, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           );

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
    AXI4_AWFlit #(id_, addr_, awuser_) awflitIn <- get (awffIn);
    vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow.aw_send, "
                       , "awflitIn ", fshow (awflitIn) ));
    // derive the new outgoing address request
    NumProxy #(out_byte_t) proxyBusW = ?;
    match {.nBytes, .awlenOut, .awsizeOut} <-
      deriveAccessParams (proxyBusW, awflitIn.awlen, awflitIn.awsize);
    let awflitOut = AXI4_AWFlit { awid: awflitIn.awid
                                , awaddr: awflitIn.awaddr
                                , awlen: awlenOut
                                , awsize: awsizeOut
                                , awburst: awflitIn.awburst
                                , awlock: awflitIn.awlock
                                , awcache: awflitIn.awcache
                                , awprot: awflitIn.awprot
                                , awqos: awflitIn.awqos
                                , awregion: awflitIn.awregion
                                , awuser: awflitIn.awuser };
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

  // local declarations
  //////////////////////////////////////////////////////////////////////////////
  // interfaces //
  ////////////////
  // Request channel, single flit
  FIFOF #(AXI4_ARFlit #(id_, addr_, aruser_)) arffIn <- mkFIFOF;
  FIFOF #(AXI4_ARFlit #(id_, addr_, aruser_)) arffOut <- mkFIFOF;
  // Data response channel
  FIFOF #(AXI4_RFlit #(id_, data_X, ruser_))
    rffIn <- mkSizedFIFOF (valueOf (buffInDepth));
  FIFOF #(AXI4_RFlit #(id_, data_Y, ruser_))
    rffOut <- mkSizedFIFOF (valueOf (buffOutDepth));
  ////////////
  // others //
  ////////////
  // local state to remember addresses
  Vector #(TExp #(id_), FIFOF #(Bit #(0))) localff <- replicateM (mkUGFIFOF);

  // handle address channel
  //////////////////////////////////////////////////////////////////////////////
  let arflitIn = arffIn.first;
  rule ar_send (localff[arflitIn.arid].notFull);
    vPrint (1, $format ("%m.mkAXI4ReadsWideToNarrow.ar_send"));
    // consume the incoming address request
    arffIn.deq;
    vPrint (2, $format ( "%m.mkAXI4ReadsWideToNarrow.ar_send, "
                       , "arflitIn ", fshow (arflitIn) ));
    // derive the new outgoing address request
    AXI4_ARFlit #(id_, addr_, aruser_) arflitOut = error ("TODO");
    // send the outgoing address request
    arffOut.enq (arflitOut);
    vPrint (2, $format ( "%m.mkAXI4ReadsWideToNarrow.ar_send, "
                       , "arflitOut ", fshow (arflitOut) ));
    // pass local information to the data channel handling rule
    Bit #(0) localpayload = error ("TODO");
    localff[arflitIn.arid].enq (localpayload);
    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow.ar_send, "
                       , "localpayload ", fshow (localpayload) ));
  endrule

  // handle data response channel
  //////////////////////////////////////////////////////////////////////////////
  let rflitOut = rffOut.first;
  rule r_accumulate_send (localff[rflitOut.rid].notEmpty);
    vPrint (1, $format ("%m.mkAXI4ReadsWideToNarrow.r_accumulate_send"));
    // read current local information
    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow.r_accumulate_send, "
                       , "localff[rflitOut.rid].first "
                       , fshow (localff[rflitOut.rid].first) ));
    //match {.addr, .nBytes} = localff[rflitOut.rid].first;
    // read and consume incoming data response flit
    rffOut.deq;
    vPrint (2, $format ( "%m.mkAXI4ReadsWideToNarrow.r_accumulate_send, "
                       , "rflitOut ", fshow (rflitOut) ));
    // accumulate the data and book-keep
    //let newData = data;
    //let newByteCnt = byteCnt;
    // when the burst is finished, consume local information
    let burstFinished = error ("TODO");
    if (burstFinished) begin
      localff[rflitOut.rid].deq;
      vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow.r_accumulate_send, "
                         , "consume localff[rflitOut.rid]" ));
    end
    // when a whole flit is ready, send it over
    let flitReady = error ("TODO");
    if (burstFinished || flitReady) begin
      AXI4_RFlit #(id_, data_X, ruser_) rflitIn = error ("TODO");
      rffIn.enq (rflitIn);
      vPrint (2, $format ( "%m.mkAXI4ReadsWideToNarrow.r_accumulate_send, "
                         , "rflitIn ", fshow (rflitIn) ));
    end
  endrule

  // return channels as interface
  //////////////////////////////////////////////////////////////////////////////
  return tuple4 ( toSink (arffIn), toSource (rffIn)
                , toSource (arffOut), toSink (rffOut) );
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
  provisos ( NumAlias #(in_bit_idx_t, TLog #(data_X))
           , NumAlias #(in_byte_idx_t, TLog #(TDiv #(data_X, 8)))
           , NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_bit_idx_t, TLog #(data_Y))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Mul #(TDiv#(data_Y, 8), 8, data_Y)
           , Add #(_a, data_X, data_Y)
           , Add #(_b, TDiv#(data_X, 8), TDiv#(data_Y, 8))
           , Add #(_c, out_byte_idx_t, MaxBytesSz)
           , Add #(_d, in_byte_idx_t, in_bit_idx_t)
           , Add #(_e, out_byte_idx_t, out_bit_idx_t)
           , Add #(_f, in_byte_idx_t, out_byte_idx_t)
           , Add #(_r, in_bit_idx_t, out_bit_idx_t)
           , Add #(_h, out_byte_idx_t, addr_)
           , Add #(_i, out_byte_idx_t, MaxBytesSz)
           , Add #(_j, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           );

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
  let reqff <- mkBypassFIFOF;

  // handle address channel
  //////////////////////////////////////////////////////////////////////////////
  rule aw_send;
    vPrint (1, $format ("%m.mkAXI4WritesNarrowToWide.aw_send"));
    // read and consume the incoming address request
    AXI4_AWFlit #(id_, addr_, awuser_) awflitIn <- get (awffIn);
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.aw_send, "
                       , "awflitIn ", fshow (awflitIn) ));
    // derive the new outgoing address request
    NumProxy #(out_byte_t) proxyBusW = ?;
    match {.nBytes, .awlenOut, .awsizeOut} <-
      deriveAccessParams (proxyBusW, awflitIn.awlen, awflitIn.awsize);
    let awflitOut = AXI4_AWFlit { awid: awflitIn.awid
                                , awaddr: awflitIn.awaddr
                                , awlen: awlenOut
                                , awsize: awsizeOut
                                , awburst: awflitIn.awburst
                                , awlock: awflitIn.awlock
                                , awcache: awflitIn.awcache
                                , awprot: awflitIn.awprot
                                , awqos: awflitIn.awqos
                                , awregion: awflitIn.awregion
                                , awuser: awflitIn.awuser };
    // send the outgoing address request
    awffOut.enq (awflitOut);
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.aw_send, "
                       , "awflitOut ", fshow (awflitOut) ));
    // pass local information to the data channel handling rule
    let reqffpayload = tuple6 ( nBytes, awflitIn.awaddr
                              , awflitIn.awsize, awflitIn.awlen
                              , awsizeOut, awlenOut );
    reqff.enq (reqffpayload);
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.aw_send, "
                       , "reqffpayload ", fshow (reqffpayload) ));
  endrule

  // handle data channel
  //////////////////////////////////////////////////////////////////////////////
  //local state
  Reg #(Bit #(MaxBytesSz)) cnt <- mkReg (0);
  Reg #(Bit #(TDiv #(data_Y, 8))) strb <- mkReg (0);
  Reg #(Bit #(data_Y)) data <- mkRegU;
  rule w_accumulate_send;
    vPrint (1, $format ("%m.mkAXI4WritesNarrowToWide.w_accumulate_send"));
    // read current local information
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "reqff.first ", fshow (reqff.first) ));
    match {.nBytes, .addr, .awsizeIn, .awlenIn, .awsizeOut, .awlenOut} =
      reqff.first;
    // consume incoming data flit
    let wflitIn <- get (wffIn);
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "wflitIn ", fshow (wflitIn) ));
    // derive the relevant data indices
    Bit #(out_byte_idx_t) width = 1 << pack (awsizeIn);
    Bit #(out_byte_idx_t) loOut = truncate (addr) + truncate (cnt);
    Bit #(in_byte_idx_t) loIn = truncate (loOut);
    Bit #(out_bit_idx_t) loOutBit = zeroExtend (loOut) << 3;
    Bit #(in_bit_idx_t) loInBit = truncate (loOutBit);
    // accumulate the data and book-keep
    Bit #(MaxBytesSz) newCnt = cnt + zeroExtend (width);
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
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "isLast ", fshow (isLast) ));
    // full flit ready
    Bit #(out_byte_idx_t) cntOffset = truncate (newCnt);
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "cntOffset ", fshow (cntOffset) ));
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
      // consume local information
      reqff.deq;
      newCnt = 0;
    end
    // accumulate state
    cnt <= newCnt;
    data <= newData;
    strb <= newStrb;
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "cnt (", fshow (cnt)
                       , ") <= newCnt (", fshow (newCnt), ")" ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "data (", fshow (data)
                       , ") <= newData (", fshow (newData), ")" ));
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "strb (", fshow (strb)
                       , ") <= newStrb (", fshow (newStrb), ")" ));
  endrule

  // return channels as interface
  //////////////////////////////////////////////////////////////////////////////
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
  provisos ( NumAlias #(in_bit_idx_t, TLog #(data_X))
           , NumAlias #(in_byte_idx_t, TLog #(TDiv #(data_X, 8)))
           , NumAlias #(out_byte_t, TDiv #(data_Y, 8))
           , NumAlias #(out_bit_idx_t, TLog #(data_Y))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Alias #(local_info, Tuple4 #( Bit #(MaxBytesSz)
                                         , Bit #(addr_)
                                         , AXI4_Size
                                         , AXI4_Len ))
           , Add #(_a, data_X, data_Y)
           , Add #(_b, out_byte_idx_t, MaxBytesSz)
           , Add #(_c, in_byte_idx_t, in_bit_idx_t)
           , Add #(_d, out_byte_idx_t, out_bit_idx_t)
           , Add #(_e, in_byte_idx_t, out_byte_idx_t)
           , Add #(_f, in_bit_idx_t, out_bit_idx_t)
           , Add #(_g, out_byte_idx_t, addr_)
           , Add #(_h, out_byte_idx_t, MaxBytesSz)
           , Add #(_i, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           );

  // local declarations
  //////////////////////////////////////////////////////////////////////////////
  // interfaces //
  ////////////////
  // Request channel, single flit
  FIFOF #(AXI4_ARFlit #(id_, addr_, aruser_)) arffIn <- mkFIFOF;
  FIFOF #(AXI4_ARFlit #(id_, addr_, aruser_)) arffOut <- mkFIFOF;
  // Data response channel
  FIFOF #(AXI4_RFlit #(id_, data_X, ruser_))
    rffIn <- mkSizedFIFOF (valueOf (buffInDepth));
  FIFOF #(AXI4_RFlit #(id_, data_Y, ruser_))
    rffOut <- mkSizedFIFOF (valueOf (buffOutDepth));
  ////////////
  // others //
  ////////////
  // local state to remember addresses
  Vector #(TExp #(id_), FIFOF #(local_info)) localff <- replicateM (mkUGFIFOF);

  // handle address channel
  //////////////////////////////////////////////////////////////////////////////
  let arflitIn = arffIn.first;
  rule ar_send (localff[arflitIn.arid].notFull);
    vPrint (1, $format ("%m.mkAXI4ReadsNarrowToWide.ar_send"));
    // consume the incoming address request
    arffIn.deq;
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.ar_send, "
                       , "arflitIn ", fshow (arflitIn) ));
    // derive the new outgoing address request
    NumProxy #(out_byte_t) proxyBusW = ?;
    match {.nBytes, .arlenOut, .arsizeOut} <-
      deriveAccessParams (proxyBusW, arflitIn.arlen, arflitIn.arsize);
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
    // send the outgoing address request
    arffOut.enq (arflitOut);
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.ar_send, "
                       , "arflitOut ", fshow (arflitOut) ));
    // pass local information to the data channel handling rule
    let localffpayload = tuple4 ( nBytes
                                , arflitIn.araddr
                                , arflitIn.arsize
                                , arflitIn.arlen );
    localff[arflitIn.arid].enq (localffpayload);
    vPrint (3, $format ( "%m.mkAXI4ReadsNarrowToWide.ar_send, "
                       , "localffpayload[%0d] "
                       , arflitIn.arid, fshow (localffpayload) ));
  endrule

  // handle data response channel
  //////////////////////////////////////////////////////////////////////////////
  Reg #(Bit #(MaxBytesSz)) cnt <- mkReg (0);
  let rflitOut = rffOut.first;
  let localffpayload = localff[rflitOut.rid].first;
  match {.nBytes, .addr, .arsize, .arlen} = localffpayload;
  rule r_accumulate_send (localff[rflitOut.rid].notEmpty);
    vPrint (1, $format ("%m.mkAXI4ReadsNarrowToWide.r_accumulate_send"));
    // read current local information
    vPrint (3, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                       , "localffpayload[%0d] "
                       , rflitOut.rid, fshow (localffpayload) ));
    // read incoming data response flit
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide.r_accumulate_send, "
                       , "rflitOut ", fshow (rflitOut) ));
    // accumulate the data and book-keep
    Bit #(out_byte_idx_t) width = 1 << pack (arsize);
    Bit #(out_byte_idx_t) loOut = truncate (addr) + truncate (cnt);
    Bit #(out_bit_idx_t) loOutBit = zeroExtend (loOut) << 3;
    Bit #(in_bit_idx_t) loInBit = truncate (loOutBit);
    Bit #(MaxBytesSz) newCnt = cnt + zeroExtend (width);
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
      localff[rflitOut.rid].deq;
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
    vPrint (2, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "cnt (", fshow (cnt)
                       , ") <= newCnt (", fshow (newCnt), ")" ));
  endrule

  // return channels as interface
  //////////////////////////////////////////////////////////////////////////////
  return tuple4 ( toSink (arffIn), toSource (rffIn)
                , toSource (arffOut), toSink (rffOut) );

endmodule

endpackage
