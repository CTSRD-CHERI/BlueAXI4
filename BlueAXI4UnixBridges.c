/*-
 * Copyright (c) 2022 Alexandre Joannou
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

#include <BlueUnixBridges.h>
#include <BlueAXI4UnixBridges.h>

inline axi4_port_fifo_desc_t* baub_fifo_OpenChannelsAsMaster
  ( char* awSrc_path, size_t awSrc_bytesize, decoder_t awSrc_decoder
  , char* wSrc_path, size_t wSrc_bytesize, decoder_t wSrc_decoder
  , char* bSnk_path, size_t bSnk_bytesize, encoder_t bSnk_encoder
  , char* arSrc_path, size_t arSrc_bytesize, decoder_t arSrc_decoder
  , char* rSnk_path, size_t rSnk_bytesize, encoder_t rSnk_encoder ) {
  axi4_port_fifo_desc_t* desc =
    (axi4_port_fifo_desc_t*) malloc (sizeof (axi4_port_fifo_desc_t));
  desc->aw =
    bub_fifo_OpenAsConsumer (awSrc_path, awSrc_bytesize, awSrc_decoder);
  desc->w =
    bub_fifo_OpenAsConsumer (wSrc_path, wSrc_bytesize, wSrc_decoder);
  desc->b =
    bub_fifo_OpenAsProducer (bSnk_path, bSnk_bytesize, bSnk_encoder);
  desc->ar =
    bub_fifo_OpenAsConsumer (arSrc_path, arSrc_bytesize, arSrc_decoder);
  desc->r =
    bub_fifo_OpenAsProducer (rSnk_path, rSnk_bytesize, rSnk_encoder);
  return desc;
}

inline axi4_port_fifo_desc_t* baub_fifo_OpenChannelsAsSlave
  ( char* awSnk_path, size_t awSnk_bytesize, encoder_t awSnk_encoder
  , char* wSnk_path, size_t wSnk_bytesize, encoder_t wSnk_encoder
  , char* bSrc_path, size_t bSrc_bytesize, decoder_t bSrc_decoder
  , char* arSnk_path, size_t arSnk_bytesize, encoder_t arSnk_encoder
  , char* rSrc_path, size_t rSrc_bytesize, decoder_t rSrc_decoder ) {
  axi4_port_fifo_desc_t* desc =
    (axi4_port_fifo_desc_t*) malloc (sizeof (axi4_port_fifo_desc_t));
  desc->aw =
    bub_fifo_OpenAsProducer (awSnk_path, awSnk_bytesize, awSnk_encoder);
  desc->w =
    bub_fifo_OpenAsProducer (wSnk_path, wSnk_bytesize, wSnk_encoder);
  desc->b =
    bub_fifo_OpenAsConsumer (bSrc_path, bSrc_bytesize, bSrc_decoder);
  desc->ar =
    bub_fifo_OpenAsProducer (arSnk_path, arSnk_bytesize, arSnk_encoder);
  desc->r =
    bub_fifo_OpenAsConsumer (rSrc_path, rSrc_bytesize, rSrc_decoder);
  return desc;
}

inline void baub_fifo_Close (axi4_port_fifo_desc_t* desc) {
  bub_fifo_Close (desc->aw);
  bub_fifo_Close (desc->w);
  bub_fifo_Close (desc->b);
  bub_fifo_Close (desc->ar);
  bub_fifo_Close (desc->r);
}
