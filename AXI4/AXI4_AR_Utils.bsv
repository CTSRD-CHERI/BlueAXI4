/*-
 * Copyright (c) 2018-2022 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
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

import BlueBasics :: *;

import AXI4_Types :: *;

import FIFOF :: *;
import SpecialFIFOs :: *;

//////////////////////////////
// AXI Read Address Channel //
////////////////////////////////////////////////////////////////////////////////

// map over flit type
////////////////////////////////////////////////////////////////////////////////

function AXI4_ARFlit #(id_out, addr, user)
  mapAXI4_ARFlit_arid (
    function Bit #(id_out) f (Bit #(id_in) a)
  , AXI4_ARFlit #(id_in, addr, user) x ) =
  AXI4_ARFlit { arid: f (x.arid)
              , araddr: x.araddr
              , arlen: x.arlen
              , arsize: x.arsize
              , arburst: x.arburst
              , arlock: x.arlock
              , arcache: x.arcache
              , arprot: x.arprot
              , arqos: x.arqos
              , arregion: x.arregion
              , aruser: x.aruser };

function AXI4_ARFlit #(id, addr_out, user)
  mapAXI4_ARFlit_araddr (
    function Bit #(addr_out) f (Bit #(addr_in) a)
  , AXI4_ARFlit #(id, addr_in, user) x ) =
  AXI4_ARFlit { arid: x.arid
              , araddr: f (x.araddr)
              , arlen: x.arlen
              , arsize: x.arsize
              , arburst: x.arburst
              , arlock: x.arlock
              , arcache: x.arcache
              , arprot: x.arprot
              , arqos: x.arqos
              , arregion: x.arregion
              , aruser: x.aruser };

function AXI4_ARFlit #(id, addr, user_out)
  mapAXI4_ARFlit_aruser (
    function Bit #(user_out) f (Bit #(user_in) a)
  , AXI4_ARFlit #(id, addr, user_in) x ) =
  AXI4_ARFlit { arid: x.arid
              , araddr: x.araddr
              , arlen: x.arlen
              , arsize: x.arsize
              , arburst: x.arburst
              , arlock: x.arlock
              , arcache: x.arcache
              , arprot: x.arprot
              , arqos: x.arqos
              , arregion: x.arregion
              , aruser: f (x.aruser) };

// typeclasses to convert to/from the flit type
////////////////////////////////////////////////////////////////////////////////

typeclass ToAXI4_ARFlit#( type t
                        , numeric type id_
                        , numeric type addr_
                        , numeric type user_)
  dependencies ( t determines (id_, addr_, user_)
               , (id_, addr_, user_) determines t );
  function AXI4_ARFlit#(id_, addr_, user_) toAXI4_ARFlit (t x);
endtypeclass

instance ToAXI4_ARFlit#(AXI4_ARFlit#(a, b, c), a, b, c);
  function toAXI4_ARFlit = id;
endinstance

typeclass FromAXI4_ARFlit#( type t
                          , numeric type id_
                          , numeric type addr_
                          , numeric type user_)
  dependencies ( t determines (id_, addr_, user_)
               , (id_, addr_, user_) determines t );
  function t fromAXI4_ARFlit (AXI4_ARFlit#(id_, addr_, user_) x);
endtypeclass

instance FromAXI4_ARFlit#(AXI4_ARFlit#(a, b, c), a, b, c);
  function fromAXI4_ARFlit = id;
endinstance

// augment master / slave Sig "on action" function
////////////////////////////////////////////////////////////////////////////////

function AXI4_AR_Master_Sig #(id_, addr_, user_)
  on_arready ( function Action f ( Bool _valid
                                 , Bool _ready
                                 , AXI4_ARFlit #(id_, addr_, user_) _flit )
             , AXI4_AR_Master_Sig #(id_, addr_, user_) m) =
  interface AXI4_AR_Master_Sig;
    method arid     = m.arid;
    method araddr   = m.araddr;
    method arlen    = m.arlen;
    method arsize   = m.arsize;
    method arburst  = m.arburst;
    method arlock   = m.arlock;
    method arcache  = m.arcache;
    method arprot   = m.arprot;
    method arqos    = m.arqos;
    method arregion = m.arregion;
    method aruser   = m.aruser;
    method arvalid  = m.arvalid;
    method arready (rdy) = action
      f (m.arvalid, rdy, AXI4_ARFlit { arid:     m.arid
                                     , araddr:   m.araddr
                                     , arlen:    m.arlen
                                     , arsize:   m.arsize
                                     , arburst:  m.arburst
                                     , arlock:   m.arlock
                                     , arcache:  m.arcache
                                     , arprot:   m.arprot
                                     , arqos:    m.arqos
                                     , arregion: m.arregion
                                     , aruser:   m.aruser });
      m.arready (rdy);
    endaction;
  endinterface;

function AXI4_AR_Slave_Sig #(id_, addr_, user_)
  on_arflit ( function Action f ( Bool _valid
                                , Bool _ready
                                , AXI4_ARFlit #(id_, addr_, user_) _flit )
            , AXI4_AR_Slave_Sig #(id_, addr_, user_) s ) =
  interface AXI4_AR_Slave_Sig;
    method arflit ( arvalid
                  , arid
                  , araddr
                  , arlen
                  , arsize
                  , arburst
                  , arlock
                  , arcache
                  , arprot
                  , arqos
                  , arregion
                  , aruser ) = action
      f ( arvalid, s.arready, AXI4_ARFlit { arid:     arid
                                          , araddr:   araddr
                                          , arlen:    arlen
                                          , arsize:   arsize
                                          , arburst:  arburst
                                          , arlock:   arlock
                                          , arcache:  arcache
                                          , arprot:   arprot
                                          , arqos:    arqos
                                          , arregion: arregion
                                          , aruser:   aruser } );
      s.arflit ( arvalid
               , arid
               , araddr
               , arlen
               , arsize
               , arburst
               , arlock
               , arcache
               , arprot
               , arqos
               , arregion
               , aruser );
    endaction;
    method arready = s.arready;
  endinterface;

// augment master / slave Sig
////////////////////////////////////////////////////////////////////////////////

module augmentAXI4_AR_Master_Sig #(
    function module #(Empty) f (Bool arvalid, Bool arready, flit_t arflit)
  , ar_master_sig m) (ar_master_sig)
  provisos ( Alias #(ar_master_sig, AXI4_AR_Master_Sig #(id_, addr_, user_))
           , Alias #(flit_t, AXI4_ARFlit #(id_, addr_, user_)) );
  let arreadyWire <- mkPulseWire;
  f (m.arvalid, arreadyWire, AXI4_ARFlit { arid:     m.arid
                                         , araddr:   m.araddr
                                         , arlen:    m.arlen
                                         , arsize:   m.arsize
                                         , arburst:  m.arburst
                                         , arlock:   m.arlock
                                         , arcache:  m.arcache
                                         , arprot:   m.arprot
                                         , arqos:    m.arqos
                                         , arregion: m.arregion
                                         , aruser:   m.aruser });
  function g (_arvalid , arready , _flit) = action
    if (arready) arreadyWire.send;
  endaction;
  return on_arready (g, m);
endmodule

module augmentAXI4_AR_Slave_Sig #(
    function module #(Empty) f (Bool arvalid, Bool arready, flit_t arflit)
  , ar_slave_sig s) (ar_slave_sig)
  provisos ( Alias #(ar_slave_sig, AXI4_AR_Slave_Sig #(id_, addr_, user_))
           , Alias #(flit_t, AXI4_ARFlit #(id_, addr_, user_)) );
  let arvalidWire <- mkPulseWire;
  let arflitWire <- mkDWire (?);
  f (arvalidWire, s.arready, arflitWire);
  function g (arvalid, _arready, arflit) = action
    if (arvalid) arvalidWire.send;
    arflitWire <= arflit;
  endaction;
  return on_arflit (g, s);
endmodule

// convert to/from Sig Master interface
////////////////////////////////////////////////////////////////////////////////

module toAXI4_AR_Master_Sig #(src_t#(t) s)
                             (AXI4_AR_Master_Sig#(id_, addr_, user_))
  provisos ( ToSource#(src_t#(t), t)
           , ToAXI4_ARFlit#(t, id_, addr_, user_)
           , Bits#(t, t_sz));
  let src <- toUnguardedSource(s, ?);
  AXI4_ARFlit#(id_, addr_, user_) flit = toAXI4_ARFlit(src.peek);
  method arid     = flit.arid;
  method araddr   = flit.araddr;
  method arlen    = flit.arlen;
  method arsize   = flit.arsize;
  method arburst  = flit.arburst;
  method arlock   = flit.arlock;
  method arcache  = flit.arcache;
  method arprot   = flit.arprot;
  method arqos    = flit.arqos;
  method arregion = flit.arregion;
  method aruser   = flit.aruser;
  method arvalid  = src.canPeek;
  method arready(rdy) = action if (src.canPeek && rdy) src.drop; endaction;
endmodule

module fromAXI4_AR_Master_Sig #(AXI4_AR_Master_Sig#(id_, addr_, user_) m)
                               (Source#(AXI4_ARFlit#(id_, addr_, user_)));
  FIFOF#(AXI4_ARFlit#(id_, addr_, user_)) buffer <- mkSizedBypassFIFOF(1);
  let snk <- toUnguardedSink(buffer);
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardFlit (m.arvalid && snk.canPut);
    snk.put (AXI4_ARFlit { arid:     m.arid
                         , araddr:   m.araddr
                         , arlen:    m.arlen
                         , arsize:   m.arsize
                         , arburst:  m.arburst
                         , arlock:   m.arlock
                         , arcache:  m.arcache
                         , arprot:   m.arprot
                         , arqos:    m.arqos
                         , arregion: m.arregion
                         , aruser:   m.aruser });
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardReady; m.arready(snk.canPut); endrule
  return toSource(buffer);
endmodule

// convert to/from Sig Slave interface
////////////////////////////////////////////////////////////////////////////////

module toAXI4_AR_Slave_Sig #(snk_t s)
                            (AXI4_AR_Slave_Sig#(id_, addr_, user_))
  provisos ( ToSink#(snk_t, t)
           , FromAXI4_ARFlit#(t, id_, addr_, user_)
           , Bits#(t, t_sz));
  let snk <- toUnguardedSink(s);
  method arflit( arvalid
               , arid
               , araddr
               , arlen
               , arsize
               , arburst
               , arlock
               , arcache
               , arprot
               , arqos
               , arregion
               , aruser) = action if (arvalid && snk.canPut)
    snk.put(fromAXI4_ARFlit(AXI4_ARFlit{ arid:     arid
                                       , araddr:   araddr
                                       , arlen:    arlen
                                       , arsize:   arsize
                                       , arburst:  arburst
                                       , arlock:   arlock
                                       , arcache:  arcache
                                       , arprot:   arprot
                                       , arqos:    arqos
                                       , arregion: arregion
                                       , aruser:   aruser }));
  endaction;
  method arready = snk.canPut;
endmodule

module fromAXI4_AR_Slave_Sig #(AXI4_AR_Slave_Sig#(id_, addr_, user_) s)
                              (Sink#(AXI4_ARFlit#(id_, addr_, user_)));
  // We use a guarded buffer to export as a guarded sink, and use an unguarded
  // source as an internal interface to it for connection to the Sig interface
  FIFOF#(AXI4_ARFlit#(id_, addr_, user_)) buffer <- mkSizedBypassFIFOF(1);
  let src <- toUnguardedSource(buffer, ?);
  (* fire_when_enabled, no_implicit_conditions *)
  rule forwardFlit;
    s.arflit( src.canPeek
            , src.peek.arid
            , src.peek.araddr
            , src.peek.arlen
            , src.peek.arsize
            , src.peek.arburst
            , src.peek.arlock
            , src.peek.arcache
            , src.peek.arprot
            , src.peek.arqos
            , src.peek.arregion
            , src.peek.aruser);
  endrule
  (* fire_when_enabled, no_implicit_conditions *)
  rule dropFlit (src.canPeek && s.arready); src.drop; endrule
  return toSink(buffer);
endmodule

// probe AR flit
////////////////////////////////////////////////////////////////////////////////

module probeAXI4_ARFlit #( Bool ar_valid
                         , Bool ar_ready
                         , AXI4_ARFlit #(id_, addr_, user_) ar_flit) (Empty);
  let arid <- mkSignalProbe (ar_flit.arid);
  let araddr <- mkSignalProbe (ar_flit.araddr);
  let arlen <- mkSignalProbe (ar_flit.arlen);
  let arsize <- mkSignalProbe (ar_flit.arsize);
  let arburst <- mkSignalProbe (ar_flit.arburst);
  let arlock <- mkSignalProbe (ar_flit.arlock);
  let arcache <- mkSignalProbe (ar_flit.arcache);
  let arprot <- mkSignalProbe (ar_flit.arprot);
  let arqos <- mkSignalProbe (ar_flit.arqos);
  let arregion <- mkSignalProbe (ar_flit.arregion);
  let aruser <- mkSignalProbe (ar_flit.aruser);
  let arvalid <- mkSignalProbe (ar_valid);
  let arready <- mkSignalProbe (ar_ready);
endmodule

// probe Master interface
////////////////////////////////////////////////////////////////////////////////

module probeAXI4_AR_Master_Sig #(AXI4_AR_Master_Sig #(id_, addr_, user_) m)
                                (AXI4_AR_Master_Sig #(id_, addr_, user_));
  augmentAXI4_AR_Master_Sig (probeAXI4_ARFlit, m);
  return m;
endmodule

module probeAXI4_ARFlit_Source #(src_t src) (Source #(flit_t))
  provisos ( Alias #(flit_t, AXI4_ARFlit #(id_, addr_, user_))
           , ToSource #(src_t, flit_t) );
  let probed <- augmentSourceWith (probeAXI4_ARFlit, src);
  return probed;
endmodule

// probe Slave interface
////////////////////////////////////////////////////////////////////////////////

module probeAXI4_AR_Slave_Sig #(AXI4_AR_Slave_Sig #(id_, addr_, user_) s)
                               (AXI4_AR_Slave_Sig #(id_, addr_, user_));
  augmentAXI4_AR_Slave_Sig (probeAXI4_ARFlit, s);
  return s;
endmodule

module probeAXI4_ARFlit_Sink #(snk_t snk) (Sink #(flit_t))
  provisos ( Alias #(flit_t, AXI4_ARFlit #(id_, addr_, user_))
           , ToSink #(snk_t, flit_t) );
  module f #(Bool canPut, Maybe #(flit_t) mData) (Empty);
    probeAXI4_ARFlit (isValid (mData), canPut, fromMaybe (?, mData));
  endmodule
  let probed <- augmentSinkWith (f, snk);
  return probed;
endmodule
