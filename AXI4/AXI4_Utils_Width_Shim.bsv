/*-
 * Copyright (c) 2018-2024 Alexandre Joannou
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

function Action die (Fmt fmt) = action
  $display ("%0t - ", $time, fmt);
  $finish;
endaction;

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
  (Tuple2 #( AXI4_Slave  #( id_t, addr_t, wide_bit_t
                          , awuser_t, wuser_t, buser_t, aruser_t, ruser_t )
           , AXI4_Master #( id_t, addr_t, narrow_bit_t
                          , awuser_t, wuser_t, buser_t, aruser_t, ruser_t )))
  provisos (
    NumAlias #(wide_byte_t, TDiv #(wide_bit_t, 8))
  , NumAlias #(narrow_byte_t, TDiv #(narrow_bit_t, 8))
  , NumAlias #(narrow_byte_idx_t, TLog #(narrow_byte_t))
  , NumAlias #(lanes_idx_t, TLog #(ratio_t))
  , NumAlias #(ratio_t, TDiv #(wide_byte_t, narrow_byte_t))
  , Mul #(ratio_t, narrow_bit_t, wide_bit_t)
  , Mul #(ratio_t, narrow_byte_t, wide_byte_t)
  , Add #(_a, lanes_idx_t, addr_t)
  , Add #(_b, TAdd #(SizeOf #(AXI4_Len), 1), addr_t)
  );

  // Write channels
  let awWide <- mkSourceSinkShimBypassFF1;
  let wWide <- mkSourceSinkShimBypassFF1;
  let bWide <- mkSourceSinkShimBypassFF1;
  let awNarrow <- mkSourceSinkShimBypassFF1;
  let wNarrow <- mkSourceSinkShimBypassFF1;
  let bNarrow <- mkSourceSinkShimBypassFF1;
  mkAXI4WritesWideToNarrow (
    tuple3 (awWide.source, wWide.source, bWide.sink)
  , tuple3 (awNarrow.sink, wNarrow.sink, bNarrow.source)
  );

  // Read channels
  let arWide <- mkSourceSinkShimBypassFF1;
  let rWide <- mkSourceSinkShimBypassFF1;
  let arNarrow <- mkSourceSinkShimBypassFF1;
  let rNarrow <- mkSourceSinkShimBypassFF1;
  mkAXI4ReadsWideToNarrow (
    tuple2 (arWide.source, rWide.sink)
  , tuple2 (arNarrow.sink, rNarrow.source)
  );

  return tuple2 (
    interface AXI4_Slave;
      interface aw = awWide.sink;
      interface  w = wWide.sink;
      interface  b = bWide.source;
      interface ar = arWide.sink;
      interface  r = rWide.source;
    endinterface
  , interface AXI4_Master;
      interface aw = awNarrow.source;
      interface  w = wNarrow.source;
      interface  b = bNarrow.sink;
      interface ar = arNarrow.source;
      interface  r = rNarrow.sink;
    endinterface );

endmodule

// AXI4 narrow to wide data shim
////////////////////////////////////////////////////////////////////////////////

module mkAXI4DataWidthShim_NarrowToWide
  // received parameters
  #( parameter NumProxy #(buffInDepth)  proxyBuffInDepth
   , parameter NumProxy #(buffOutDepth) proxyBuffOutDepth )
  // returned interface
  (Tuple2 #( AXI4_Slave  #( id_, addr_, in_bit_t
                          , awuser_, wuser_, buser_, aruser_, ruser_ )
           , AXI4_Master #( id_, addr_, out_bit_t
                          , awuser_, wuser_, buser_, aruser_, ruser_ )))
  provisos ( NumAlias #(out_byte_t, TDiv #(out_bit_t, 8))
           , NumAlias #(out_bit_idx_t, TLog #(out_bit_t))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , NumAlias #(in_byte_t, TDiv #(in_bit_t, 8))
           , NumAlias #(in_bit_idx_t, TLog #(in_bit_t))
           , NumAlias #(in_byte_idx_t, TLog #(in_byte_t))
           , NumAlias #(ratio_t, TDiv #(out_bit_t, in_bit_t))
           , Mul #(out_byte_t, 8, out_bit_t)
           , Mul #(ratio_t, in_bit_t, out_bit_t)
           , Add #(_a, in_bit_t, out_bit_t)
           , Add #(_b, in_byte_t, out_byte_t)
           , Add #(_c, in_byte_idx_t, out_byte_idx_t)
           , Add #(_d, in_byte_idx_t, in_bit_idx_t)
           , Add #(_e, out_byte_idx_t, out_bit_idx_t)
           , Add #(_f, out_byte_idx_t, addr_)
           , Add #(_g, in_bit_idx_t, out_bit_idx_t)
           , Add #(_h, out_byte_idx_t, MaxBytesSz)
           , Add #(_i, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           , Add #(_j, TLog #(ratio_t), addr_)
           , Add #(_k, TAdd #(SizeOf #(AXI4_Len), 1), addr_)
           );
  match {.aw_X, .w_X, .b_X, .aw_Y, .w_Y, .b_Y}
    <- mkAXI4WritesNarrowToWide (proxyBuffInDepth, proxyBuffOutDepth);
  let arNarrow <- mkSourceSinkShimBypassFF1;
  let rNarrow <- mkSourceSinkShimBypassFF1;
  let arWide <- mkSourceSinkShimBypassFF1;
  let rWide <- mkSourceSinkShimBypassFF1;
  mkAXI4ReadsNarrowToWide ( tuple2 (arNarrow.source, rNarrow.sink)
                          , tuple2 (arWide.sink, rWide.source) );
  return tuple2 (
    interface AXI4_Slave;
      interface aw = aw_X;
      interface  w = w_X;
      interface  b = b_X;
      interface ar = arNarrow.sink;
      interface  r = rNarrow.source;
    endinterface
  , interface AXI4_Master;
      interface aw = aw_Y;
      interface  w = w_Y;
      interface  b = b_Y;
      interface ar = arWide.source;
      interface  r = rWide.sink;
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
  deriveAccessParams ( NumProxy #(dstBusByteW) dstProxy // dest bus byte width
                     , AXI4_Len lenIn // original AXI4 len
                     , AXI4_Size sizeIn // original AXI4 size
                     )
  provisos ( NumAlias #(dstBusOffset_t, TLog #(dstBusByteW))
           , NumAlias #(flitIdx_t, TSub #(MaxBytesSz, dstBusOffset_t))
           , Add #(_a, dstBusOffset_t, MaxBytesSz)
           , Add #(_b, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, dstBusOffset_t))
           ) = actionvalue
  // compute number of bytes in the access
  ///////////////////////////////////////////////////////////
  Bit #(MaxBytesSz) nBytes = (zeroExtend (lenIn) + 1) << pack (sizeIn);

  // early bypass in the case the incoming size is narrower than the destination
  // bus width
  Bit #(MaxBytesSz) byteWidthIn = 1 << pack(sizeIn);
  if (byteWidthIn <= fromInteger(valueOf(dstBusByteW)))
    return tuple3 (nBytes, lenIn, sizeIn);
  else begin
    // compute derived values
    ///////////////////////////////////////////////////////////
    Bit #(dstBusOffset_t) overflow = truncate (nBytes);
    Bit #(flitIdx_t) nFlits = truncateLSB (nBytes);
    Bool fitsInDstBus = nBytes <= fromInteger (valueOf (dstBusByteW));
    vPrint (4, $format ("%m.deriveAccessParams - nBytes: %0d", nBytes));
    vPrint (4, $format ("%m.deriveAccessParams - overflow: %0d", overflow));
    vPrint (4, $format ("%m.deriveAccessParams - nFlits: %0d", nFlits));
    vPrint (4, $format ( "%m.deriveAccessParams - fitsInDstBus: "
                       , fshow (fitsInDstBus) ));
    // derive new AXI4 len and size
    ///////////////////////////////
    AXI4_Len lenOut = fitsInDstBus ? 0 : truncate (nFlits - 1);
    AXI4_Size sizeOut = ?;
    case (toAXI4_Size (truncate (nBytes))) matches
      tagged Valid .x &&& fitsInDstBus: sizeOut = x;
      .* &&& (overflow != 0): begin
        sizeOut = sizeIn;
        lenOut = lenIn;
      end
      .* &&& (!fitsInDstBus):
        sizeOut = toAXI4_Size (fromInteger (valueOf (dstBusByteW))).Valid;
      default: die ($format ("error: unsupported AXI4 size encountered"));
    endcase
    vPrint (4, $format ("%m.deriveAccessParams - lenOut: %0d", lenOut));
    vPrint (4, $format ("%m.deriveAccessParams - sizeOut: %0d", sizeOut));

    // a few assertions
    ///////////////////
    if (overflow == 0 && nFlits == 0)
      die ($format ("error: encountered AXI4 transfer with 0 flits"));
    if ((overflow != 0 && nFlits > 255) || nFlits > 256)
      die ($format ("error: too long AXI4 transfer (>256 flits) encountered"));
    if (!isPowerOf2 (valueOf (dstBusByteW)))
      die ($format ("desired bus width should be a power of 2"));

    // return results
    /////////////////
    return tuple3 (nBytes, lenOut, sizeOut);
  end
endactionvalue;

// Convert wide writes to narrow writes
////////////////////////////////////////////////////////////////////////////////

// XXX TODO: for the time being, this module does not break atomicity of
// transactions and limits itself to one request in the destination AXI4 domain
// per incoming request. Since the incoming requests are on a wider bus, it may
// be impossible to fit the total requested amount of bytes in a single request
// of the destination AXI4 domain. Such a request is explicitly not supported.
// (undefined behaviour in hardware, assertion in simulation)
module mkAXI4WritesWideToNarrow
  // received interfaces
  #( Tuple3 #( Source #(AXI4_AWFlit #(id_t, addr_t, awuser_t))
             , Source #(AXI4_WFlit #(wide_bit_t, wuser_t))
             , Sink #(AXI4_BFlit #(id_t, buser_t)) ) wide
   , Tuple3 #( Sink #(AXI4_AWFlit #(id_t, addr_t, awuser_t))
             , Sink #(AXI4_WFlit #(narrow_bit_t, wuser_t))
             , Source #(AXI4_BFlit #(id_t, buser_t)) ) narrow )
  // returned interface
  (Empty)
  provisos (
    NumAlias #(wide_byte_t, TDiv #(wide_bit_t, 8))
  , NumAlias #(narrow_byte_t, TDiv #(narrow_bit_t, 8))
  , NumAlias #(narrow_byte_idx_t, TLog #(narrow_byte_t))
  , NumAlias #(lanes_idx_t, TLog #(ratio_t))
  , NumAlias #(ratio_t, TDiv #(wide_byte_t, narrow_byte_t))
  , Mul #(ratio_t, narrow_bit_t, wide_bit_t)
  , Mul #(ratio_t, narrow_byte_t, wide_byte_t)
  , Add #(_a, lanes_idx_t, addr_t)
  , Add #(_b, TAdd #(SizeOf #(AXI4_Len), 1), addr_t)
  );

  // extract handles to received interfaces
  match {.awWideSrc, .wWideSrc, .bWideSnk} = wide;
  match {.awNarrowSnk, .wNarrowSnk, .bNarrowSrc} = narrow;

  // internal bookkeeping fifo
  FIFOF #(Tuple3 #(
    Bit #(addr_t) // wide requested address
  , AXI4_Size     // wide requested size
  , AXI4_Size     // narrow requested size
  )) ff <- mkUGFIFOF;

  // maximum size supported by the narrow bus
  AXI4_Size maxNarrowSize =
    toAXI4_Size (fromInteger (valueOf (narrow_byte_t))).Valid;

  //////////////////////////////////////////////////////////////////////////////
  let awflit = awWideSrc.peek;
  rule forward_aw_flit (ff.notFull && awWideSrc.canPeek && awNarrowSnk.canPut);
    vPrint (2, $format ("%m.mkAXI4WritesWideToNarrow - forward_aw_flit"));
    vPrint (1, $format ("%m.mkAXI4WritesWideToNarrow - wide: ", fshow (awflit)));
    // consume wide aw flit, init narrow addr, size and len
    awWideSrc.drop;
    Bit #(addr_t) narrowAddr = awflit.awaddr;
    AXI4_Size narrowSize = awflit.awsize;
    AXI4_Len narrowLen = awflit.awlen;
    // if the requested size doesn't fit, ask for more flits
    if (awflit.awsize > maxNarrowSize) begin
      vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow - size doesn't fit"
                         , ", saturate at ", fshow (maxNarrowSize) ));
      narrowAddr = awflit.awaddr & (~0 << pack (awflit.awsize));
      narrowSize = maxNarrowSize;
      Bit #(SizeOf #(AXI4_Len)) tmp =
        ~(~narrowLen << (pack (awflit.awsize) - pack (maxNarrowSize)));
      if (msb (tmp) == 1'b1) die ($format("unsupported AXI4_Len ", fshow(tmp)));
      else narrowLen = truncate (tmp);
    end
    // bookkeep
    ff.enq (tuple3 (narrowAddr, awflit.awsize , narrowSize));
    // send the new narrow ar flit
    let awflitNarrow = AXI4_AWFlit { awid: awflit.awid
                                   , awaddr: narrowAddr
                                   , awlen: narrowLen
                                   , awsize: narrowSize
                                   , awburst: awflit.awburst
                                   , awlock: awflit.awlock
                                   , awcache: awflit.awcache
                                   , awprot: awflit.awprot
                                   , awqos: awflit.awqos
                                   , awregion: awflit.awregion
                                   , awuser: awflit.awuser };
    awNarrowSnk.put (awflitNarrow);
    vPrint (1, $format ( "%m.mkAXI4WritesWideToNarrow - narrow: "
                       , fshow (awflitNarrow) ));
  endrule

  //////////////////////////////////////////////////////////////////////////////
  match {.narrowAddr, .wideSize, .narrowSize} = ff.first;
  Reg #(Bit #(TAdd #(SizeOf #(AXI4_Len), 1))) flitCnt <- mkReg (0);
  rule forward_w_flit (ff.notEmpty && wWideSrc.canPeek && wNarrowSnk.canPut);
    vPrint (1, $format ( "%m.mkAXI4WritesWideToNarrow - forward_w_flit"));
    let wideFlit = wWideSrc.peek;
    vPrint (1, $format ( "%m.mkAXI4WritesWideToNarrow - wide: "
                       , fshow (wideFlit) ));
    let mask = ~(~0 << (pack (wideSize) - pack (narrowSize)));
    let wideFlitDone = (flitCnt & mask) == mask;
    let newFlitCnt = flitCnt + 1;
    // identify relevant lanes index
    Bit #(addr_t) currentAddr =
      narrowAddr + (zeroExtend (flitCnt) << pack (narrowSize));
    Bit #(lanes_idx_t) lanes_idx =
      truncate (currentAddr >> valueOf (narrow_byte_idx_t));
    vPrint (1, $format ( "%m.mkAXI4WritesWideToNarrow "
                       , "- narrowAddr: ", fshow (narrowAddr)
                       , ", wideSize: ", fshow (wideSize)
                       , ", narrowSize: ", fshow (narrowSize) ));
    vPrint (1, $format ( "%m.mkAXI4WritesWideToNarrow "
                       , "- flitCnt: ", fshow (flitCnt)
                       , "- newFlitCnt: ", fshow (newFlitCnt)
                       , ", currentAddr: ", fshow (currentAddr)
                       , ", lanes_idx: ", fshow (lanes_idx) ));
    Vector #(ratio_t, Bit #(narrow_bit_t)) data = unpack (wideFlit.wdata);
    Vector #(ratio_t, Bit #(narrow_byte_t)) strb = unpack (wideFlit.wstrb);
    let narrowFlit = AXI4_WFlit { wdata: data[lanes_idx]
                                , wstrb: strb[lanes_idx]
                                , wlast: wideFlit.wlast && wideFlitDone
                                , wuser: wideFlit.wuser
                                };
    wNarrowSnk.put (narrowFlit);
    vPrint (1, $format ( "%m.mkAXI4WritesWideToNarrow - narrow: "
                       , fshow (narrowFlit) ));
    vPrint (2, $format ( "%m.mkAXI4WritesWideToNarrow - wideFlitDone: "
                       , fshow (wideFlitDone) ));
    if (wideFlitDone) begin
      vPrint (3, $format ( "%m.mkAXI4WritesWideToNarrow - consume wide w flit"));
      wWideSrc.drop;
      if (wideFlit.wlast) begin
        vPrint (3, $format ( "%m.mkAXI4WritesWideToNarrow - last w flit"));
        ff.deq;
        newFlitCnt = 0;
      end
    end
    flitCnt <= newFlitCnt;
  endrule

  //////////////////////////////////////////////////////////////////////////////
  rule forward_b_flit (bNarrowSrc.canPeek && bWideSnk.canPut);
    let bflit <- get (bNarrowSrc);
    bWideSnk.put (bflit);
  endrule

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
           , Sink #(AXI4_WFlit #(in_bit_t, wuser_))
           , Source #(AXI4_BFlit #(id_, buser_))
           , Source #(AXI4_AWFlit #(id_, addr_, awuser_))
           , Source #(AXI4_WFlit #(out_bit_t, wuser_))
           , Sink #(AXI4_BFlit #(id_, buser_)) ))
  provisos ( NumAlias #(in_bit_idx_t, TLog #(in_bit_t))
           , NumAlias #(in_byte_idx_t, TLog #(TDiv #(in_bit_t, 8)))
           , NumAlias #(out_byte_t, TDiv #(out_bit_t, 8))
           , NumAlias #(out_bit_idx_t, TLog #(out_bit_t))
           , NumAlias #(out_byte_idx_t, TLog #(out_byte_t))
           , Mul #(TDiv#(out_bit_t, 8), 8, out_bit_t)
           , Div #(in_bit_t, 8, in_byte_t)
           , Add #(_a, in_bit_t, out_bit_t)
           , Add #(_b, TDiv#(in_bit_t, 8), TDiv#(out_bit_t, 8))
           , Add #(_c, out_byte_idx_t, MaxBytesSz)
           , Add #(_d, in_byte_t, out_byte_t)
           , Add #(_e, in_byte_idx_t, in_bit_idx_t)
           , Add #(_f, out_byte_idx_t, out_bit_idx_t)
           , Add #(_g, in_byte_idx_t, out_byte_idx_t)
           , Add #(_h, in_bit_idx_t, out_bit_idx_t)
           , Add #(_i, out_byte_idx_t, addr_)
           , Add #(_j, out_byte_idx_t, MaxBytesSz)
           , Add #(_k, SizeOf #(AXI4_Len), TSub #(MaxBytesSz, out_byte_idx_t))
           );

  // local declarations
  //////////////////////////////////////////////////////////////////////////////
  // interfaces //
  ////////////////
  // Address request channel, altered to account for burst size changes
  FIFOF #(AXI4_AWFlit #(id_, addr_, awuser_)) awffIn <- mkFIFOF;
  FIFOF #(AXI4_AWFlit #(id_, addr_, awuser_)) awffOut <- mkFIFOF;
  // Data request channel
  FIFOF #(AXI4_WFlit #(in_bit_t, wuser_))
    wffIn <- mkSizedFIFOF (valueOf (buffInDepth));
  FIFOF #(AXI4_WFlit #(out_bit_t, wuser_))
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
    NumProxy #(out_byte_t) proxyDstBusW = error("Don't look inside a proxy");
    match {.nBytes, .awlenOut, .awsizeOut} <-
      deriveAccessParams (proxyDstBusW, awflitIn.awlen, awflitIn.awsize);
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
  Reg #(Bit #(out_byte_t)) strb <- mkReg (0);
  Reg #(Bit #(out_bit_t)) data <- mkRegU;
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
    Bit #(out_byte_t) msk = ~(~0 << width) << loOut;
    Bit #(in_byte_t) tmpStrbIn = wflitIn.wstrb >> loIn;
    Bit #(out_byte_t) tmpStrbOut = zeroExtend (tmpStrbIn) << loOut;
    Bit #(out_byte_t) newStrb = mergeWithMask (msk, strb, tmpStrbOut);
    Bit #(in_bit_t) tmpDataIn = wflitIn.wdata >> loInBit;
    Bit #(out_bit_t) tmpDataOut = zeroExtend (tmpDataIn) << loOutBit;
    Bit #(out_bit_t) newData = mergeWithBE (msk, data, tmpDataOut);
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
    Bit #(out_byte_idx_t) cntMask = ~(~0 << pack (awsizeOut));
    vPrint (3, $format ( "%m.mkAXI4WritesNarrowToWide.w_accumulate_send, "
                       , "cntOffset ", fshow (cntOffset) ));
    if (isLast || (cntOffset & cntMask) == 0) begin
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

// Convert wide reads to narrow reads
////////////////////////////////////////////////////////////////////////////////

// XXX TODO: for the time being, this module does not break atomicity of
// transactions and limits itself to one request in the destination AXI4 domain
// per incoming request. Since the incoming requests are on a wider bus, it may
// be impossible to fit the total requested amount of bytes in a single request
// of the destination AXI4 domain. Such a request is explicitly not supported.
// (undefined behaviour in hardware, assertion in simulation)
module mkAXI4ReadsWideToNarrow
  // received interfaces
  #( Tuple2 #( Source #(AXI4_ARFlit #(id_t, addr_t, aruser_t))
             , Sink #(AXI4_RFlit #(id_t, wide_bit_t, ruser_t)) ) wide
   , Tuple2 #( Sink #(AXI4_ARFlit #(id_t, addr_t, aruser_t))
             , Source #(AXI4_RFlit #(id_t, narrow_bit_t, ruser_t)) ) narrow )
  (Empty)
  provisos (
    NumAlias #(wide_byte_t, TDiv #(wide_bit_t, 8))
  , NumAlias #(wide_byte_idx_t, TLog #(wide_byte_t))
  , NumAlias #(narrow_byte_t, TDiv #(narrow_bit_t, 8))
  , NumAlias #(narrow_byte_idx_t, TLog #(narrow_byte_t))
  , NumAlias #(ratio_t, TDiv #(wide_byte_t, narrow_byte_t))
  , NumAlias #(lanes_idx_t, TLog #(ratio_t))
  , Mul #(ratio_t, narrow_bit_t, wide_bit_t)
  , Add #(_a, lanes_idx_t, addr_t)
  , Add #(_b, TAdd #(SizeOf #(AXI4_Len), 1), addr_t)
  );

  // extract handles to received interfaces
  match {.arWideSrc, .rWideSnk} = wide;
  match {.arNarrowSnk, .rNarrowSrc} = narrow;

  // internal bookkeeping fifos
  Vector #(TExp #(id_t), FIFOF #(Tuple3 #(
    Bit #(addr_t) // narrow requested address
  , AXI4_Size     // wide requested size
  , AXI4_Size     // narrow requested size
  ))) ffs <- replicateM (mkUGFIFOF);

  // maximum size supported by the narrow bus
  AXI4_Size maxNarrowSize =
    toAXI4_Size (fromInteger (valueOf (narrow_byte_t))).Valid;

  // forward ar flits
  // if the requested size does not fit in the narrow bus, adapt the request to
  // take the bus width ratio into account
  let arflit = arWideSrc.peek;
  rule forward_ar_flit (   ffs[arflit.arid].notFull
                        && arWideSrc.canPeek
                        && arNarrowSnk.canPut );
    vPrint (2, $format ("%m.mkAXI4ReadsWideToNarrow - forward_ar_flit"));
    vPrint (1, $format ("%m.mkAXI4ReadsWideToNarrow - wide: ", fshow (arflit)));
    // consume wide ar flit, init narrow addr, size and len
    arWideSrc.drop;
    Bit #(addr_t) narrowAddr = arflit.araddr;
    AXI4_Size narrowSize = arflit.arsize;
    AXI4_Len narrowLen = arflit.arlen;
    // if the requested size doesn't fit, ask for more flits
    if (arflit.arsize > maxNarrowSize) begin
      vPrint (2, $format ( "%m.mkAXI4ReadsWideToNarrow - size doesn't fit"
                         , ", saturate at ", fshow (maxNarrowSize) ));
      narrowAddr = arflit.araddr & (~0 << pack (arflit.arsize));
      narrowSize = maxNarrowSize;
      //Bit #(TAdd #(SizeOf #(AXI4_Len), 1)) tmp = (zeroExtend (narrowLen) + 1);
      //tmp = tmp << (pack (arflit.arsize) - pack (maxNarrowSize));
      //tmp = tmp - 1;
      Bit #(SizeOf #(AXI4_Len)) tmp =
        ~(~narrowLen << (pack (arflit.arsize) - pack (maxNarrowSize)));
      if (msb (tmp) == 1'b1) die ($format("unsupported AXI4_Len ", fshow(tmp)));
      else narrowLen = truncate (tmp);
    end
    // bookkeep
    ffs[arflit.arid].enq (tuple3 (
      narrowAddr
    , arflit.arsize
    , narrowSize ));
    // send the new narrow ar flit
    let arflitNarrow = AXI4_ARFlit { arid: arflit.arid
                                   , araddr: narrowAddr
                                   , arlen: narrowLen
                                   , arsize: narrowSize
                                   , arburst: arflit.arburst
                                   , arlock: arflit.arlock
                                   , arcache: arflit.arcache
                                   , arprot: arflit.arprot
                                   , arqos: arflit.arqos
                                   , arregion: arflit.arregion
                                   , aruser: arflit.aruser };
    arNarrowSnk.put (arflitNarrow);
    vPrint (1, $format ( "%m.mkAXI4ReadsWideToNarrow - narrow: "
                       , fshow (arflitNarrow) ));
  endrule

  // forward r flits (accumulate if necessary)
  // XXX assumes well-formed r transfer (with consistent rid througout)
  AXI4_RFlit #(id_t, narrow_bit_t, ruser_t) rflit = rNarrowSrc.peek;
  match {.narrowAddr, .wideSize, .narrowSize} = ffs[rflit.rid].first;
  Reg #(Bit #(TAdd #(SizeOf #(AXI4_Len), 1))) flitCnt <- mkReg (0);
  Reg #(Vector #(ratio_t, Bit #(narrow_bit_t))) data <- mkRegU; // accumulated data

  rule accumulate_r_flit (   ffs[rflit.rid].notEmpty
                          && rWideSnk.canPut
                          && rNarrowSrc.canPeek );
    vPrint (2, $format ("%m.mkAXI4ReadsWideToNarrow - accumulate_r_flit"));
    vPrint (1, $format ( "%m.mkAXI4ReadsWideToNarrow - narrow: "
                       , fshow (rflit) ));
    // consume narrow r flit
    rNarrowSrc.drop;
    let newFlitCnt = flitCnt + 1;
    // identify relevant lane index to produce data and accumulate the data
    Bit #(addr_t) currentAddr =
      narrowAddr + (zeroExtend (flitCnt) << pack (narrowSize));
    Bit #(lanes_idx_t) lanes_idx =
      truncate (currentAddr >> valueOf (narrow_byte_idx_t));
    Vector #(ratio_t, Bit #(narrow_bit_t)) newData = data;
    newData[lanes_idx] = rflit.rdata;
    vPrint (2, $format ("%m.mkAXI4ReadsWideToNarrow - data: ", fshow (data)));
    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow - ffs[%0d] ", rflit.rid
                       , "- narrowAddr: ", fshow (narrowAddr)
                       , ", wideSize: ", fshow (wideSize)
                       , ", narrowSize: ", fshow (narrowSize)
                       ));
    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow "
                       , "- flitCnt: ", fshow (flitCnt)
                       , ", newFlitCnt: ", fshow (newFlitCnt) ));
    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow "
                       , "- currentAddr: ", fshow (currentAddr) ));
    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow - lanes_idx: "
                       , fshow (lanes_idx) ));
    Bit #(addr_t) nextAddr =
      narrowAddr + (zeroExtend (newFlitCnt) << pack (narrowSize));
    let sendWideFlit = (pack (wideSize) > pack (narrowSize)) ?
      (nextAddr & ~(~0 << pack (wideSize))) == 0 : True;
    if (sendWideFlit) begin
      // send r flit with new data
      let newRFlit = AXI4_RFlit { rid: rflit.rid
                                , rdata: pack (newData)
                                , rresp: rflit.rresp
                                , rlast: rflit.rlast
                                , ruser: rflit.ruser };
      rWideSnk.put (newRFlit);
      vPrint (1, $format ( "%m.mkAXI4ReadsWideToNarrow - wide: "
                         , fshow (newRFlit) ));
      // drain bookkeeping and reset flitCnt on last flit
      if (rflit.rlast) begin
        ffs[rflit.rid].deq;
        newFlitCnt = 0;
        newData = unpack (~0); // XXX for debug... TODO remove
      end
    end
    // update flit counter
    flitCnt <= newFlitCnt;
    data <= newData;

    vPrint (3, $format ( "%m.mkAXI4ReadsWideToNarrow - flitCnt: "
                       , fshow (flitCnt)
                       , ", newFlitCnt: ", fshow (newFlitCnt) ));
    vPrint (2, $format ( "%m.mkAXI4ReadsWideToNarrow - newData: "
                       , fshow (newData) ));
  endrule

endmodule

// Convert narrow reads to wide reads
////////////////////////////////////////////////////////////////////////////////

// XXX TODO: currently supports arburst == INCR
//           undefined behaviour for WRAP and FIXED (in practice same as INCR)

module mkAXI4ReadsNarrowToWide
  // received interfaces
  #( Tuple2 #( Source #(AXI4_ARFlit #(id_t, addr_t, aruser_t))
             , Sink #(AXI4_RFlit #(id_t, narrow_bit_t, ruser_t)) ) narrow
   , Tuple2 #( Sink #(AXI4_ARFlit #(id_t, addr_t, aruser_t))
             , Source #(AXI4_RFlit #(id_t, wide_bit_t, ruser_t)) ) wide )
  (Empty)
  provisos (
    NumAlias #(wide_byte_t, TDiv #(wide_bit_t, 8))
  , NumAlias #(wide_byte_idx_t, TLog #(wide_byte_t))
  , NumAlias #(narrow_byte_t, TDiv #(narrow_bit_t, 8))
  , NumAlias #(narrow_byte_idx_t, TLog #(narrow_byte_t))
  , NumAlias #(ratio_t, TDiv #(wide_byte_t, narrow_byte_t))
  , NumAlias #(lanes_idx_t, TLog #(ratio_t))
  , Mul #(ratio_t, narrow_bit_t, wide_bit_t)
  , Add #(_a, lanes_idx_t, addr_t)
  , Add #(_b, TAdd #(SizeOf #(AXI4_Len), 1), addr_t)
  );

  // extract handles to interfaces
  match {.arNarrowSrc, .rNarrowSnk} = narrow;
  match {.arWideSnk, .rWideSrc} = wide;
  // internal bookkeeping fifos
  Vector #( TExp #(id_t)
          , FIFOF #(AXI4_ARFlit #(id_t, addr_t, aruser_t)))
    ffs <- replicateM (mkUGFIFOF);

  // forward ar flits (no changes)
  let arflit = arNarrowSrc.peek; // grab a handle on narrow ar flit
  rule forward_ar_flit (   ffs[arflit.arid].notFull
                        && arNarrowSrc.canPeek
                        && arWideSnk.canPut );
    vPrint (2, $format ("%m.mkAXI4ReadsNarrowToWide - forward_ar_flit"));
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide "
                       , "- ", fshow (arflit) ));
    // consume, bookkeep and produce narrow ar flit
    arNarrowSrc.drop;
    ffs[arflit.arid].enq(arflit);
    arWideSnk.put (arflit);
  endrule

  // forward r flits (no changes)
  // XXX assumes well-formed r transfer (with consistent rid throughout)
  AXI4_RFlit #(id_t, wide_bit_t, ruser_t) rflit = rWideSrc.peek; // rflit handle
  let bookkeep = ffs[rflit.rid].first; // bookkeeping handle
  Reg #(Bit #(TAdd #(SizeOf #(AXI4_Len), 1))) flitCnt <- mkReg (0);
  rule forward_r_flit ( ffs[rflit.rid].notEmpty
                       && rWideSrc.canPeek
                       && rNarrowSnk.canPut );
    vPrint (2, $format ("%m.mkAXI4ReadsNarrowToWide - forward_r_flit"));
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide "
                       , "- wide: ", fshow (rflit) ));
    let newFlitCnt = flitCnt + 1;
    // consume wide r flit, drain bookkeeping and reset flitCnt on last flit
    rWideSrc.drop;
    if (rflit.rlast) begin
      ffs[rflit.rid].deq;
      newFlitCnt = 0;
    end
    // identify relevant lane to extract data, produce and count r flit
    Bit #(addr_t) currentAddr =
      bookkeep.araddr + (zeroExtend (flitCnt) << pack (bookkeep.arsize));
    Bit #(lanes_idx_t) lanes_idx =
      truncate (currentAddr >> valueOf (narrow_byte_idx_t));
    Vector #(ratio_t, Bit #(narrow_bit_t)) data = unpack (rflit.rdata);
    let narrowFlit = AXI4_RFlit { rid: rflit.rid
                                , rdata: data[lanes_idx]
                                , rresp: rflit.rresp
                                , rlast: rflit.rlast
                                , ruser: rflit.ruser };
    rNarrowSnk.put (narrowFlit);
    vPrint (2, $format ( "%m.mkAXI4ReadsNarrowToWide "
                       , "- narrow: ", fshow (narrowFlit) ));
    flitCnt <= newFlitCnt;
  endrule

endmodule

endpackage
