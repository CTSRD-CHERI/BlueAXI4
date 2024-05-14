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

  // wide shims
  let awWide <- mkSourceSinkShimBypassFF1;
  let  wWide <- mkSourceSinkShimBypassFF1;
  let  bWide <- mkSourceSinkShimBypassFF1;
  let arWide <- mkSourceSinkShimBypassFF1;
  let  rWide <- mkSourceSinkShimBypassFF1;

  // narrow shims
  let awNarrow <- mkSourceSinkShimBypassFF1;
  let  wNarrow <- mkSourceSinkShimBypassFF1;
  let  bNarrow <- mkSourceSinkShimBypassFF1;
  let arNarrow <- mkSourceSinkShimBypassFF1;
  let  rNarrow <- mkSourceSinkShimBypassFF1;

  // connect up write channels
  mkAXI4WritesWideToNarrow (
    tuple3 (awWide.source, wWide.source, bWide.sink)
  , tuple3 (awNarrow.sink, wNarrow.sink, bNarrow.source)
  );

  // connect up read channels
  mkAXI4ReadsWideToNarrow (
    tuple2 (arWide.source, rWide.sink)
  , tuple2 (arNarrow.sink, rNarrow.source)
  );

  // export interfaces
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

module mkAXI4WritesNarrowToWide
  // received interfaces
  #( Tuple3 #( Source #(AXI4_AWFlit #(id_t, addr_t, awuser_t))
             , Source #(AXI4_WFlit #(narrow_bit_t, wuser_t))
             , Sink #(AXI4_BFlit #(id_t, buser_t)) ) narrow
   , Tuple3 #( Sink #(AXI4_AWFlit #(id_t, addr_t, awuser_t))
             , Sink #(AXI4_WFlit #(wide_bit_t, wuser_t))
             , Source #(AXI4_BFlit #(id_t, buser_t)) ) wide )
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
  match {.awNarrowSrc, .wNarrowSrc, .bNarrowSnk} = narrow;
  match {.awWideSnk, .wWideSnk, .bWideSrc} = wide;

  let ff <- mkUGFIFOF;

  //////////////////////////////////////////////////////////////////////////////
  rule forward_aw_flit (ff.notFull && awNarrowSrc.canPeek && awWideSnk.canPut);
    let awflit <- get (awNarrowSrc);
    awWideSnk.put (awflit);
    ff.enq(awflit);
  endrule

  //////////////////////////////////////////////////////////////////////////////
  Reg #(Bit #(TAdd #(SizeOf #(AXI4_Len), 1))) flitCnt <- mkReg (0);
  rule forward_w_flit (ff.notEmpty && wNarrowSrc.canPeek && wWideSnk.canPut);
    let awflit = ff.first;
    let narrowFlit <- get (wNarrowSrc);
    // identify relevant lanes index
    Bit #(addr_t) currentAddr =
      awflit.awaddr + (zeroExtend (flitCnt) << pack (awflit.awsize));
    Bit #(lanes_idx_t) lanes_idx =
      truncate (currentAddr >> valueOf (narrow_byte_idx_t));
    Vector #(ratio_t, Bit #(narrow_bit_t)) wideData = ?;
    Vector #(ratio_t, Bit #(narrow_byte_t)) wideStrb = unpack (0);
    wideData[lanes_idx] = narrowFlit.wdata;
    wideStrb[lanes_idx] = narrowFlit.wstrb;
    let wideFlit = AXI4_WFlit { wdata: pack (wideData)
                              , wstrb: pack (wideStrb)
                              , wlast: narrowFlit.wlast
                              , wuser: narrowFlit.wuser
                              };
    wWideSnk.put (wideFlit);
    let newFlitCnt = flitCnt + 1;
    if (narrowFlit.wlast) begin
      ff.deq;
      newFlitCnt = 0;
    end
    flitCnt <= newFlitCnt;
  endrule

  //////////////////////////////////////////////////////////////////////////////
  rule forward_b_flit (bWideSrc.canPeek && bNarrowSnk.canPut);
    let bflit <- get (bWideSrc);
    bNarrowSnk.put (bflit);
  endrule


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
