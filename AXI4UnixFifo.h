#ifndef AXI4_UNIX_FIFO_H
#define AXI4_UNIX_FIFO_H

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

// This header declares the BlueUnixFifo API for C client, as well as the C
// functions to be wrapped in Bluespec SystemVerilog BDPI calls for use in
// BSV simulator code.

#include <BlueUnixBridges.h>

// An AXI4UnixFifo port descriptor
typedef struct {
  fifo_desc_t* aw;
  fifo_desc_t*  w;
  fifo_desc_t*  b;
  fifo_desc_t* ar;
  fifo_desc_t*  r;
} axi4_port_fifo_desc_t;

// AXI4 Unix Bridges Fifo API
////////////////////////////////////////////////////////////////////////////////

#ifdef __cplusplus
extern "C" {
#endif

extern axi4_port_fifo_desc_t* aub_fifo_OpenAsMaster
  ( char* awSrc_path, size_t awSrc_bytesize, decoder_t awSrc_decoder
  , char* wSrc_path, size_t wSrc_bytesize, decoder_t wSrc_decoder
  , char* bSnk_path, size_t bSnk_bytesize, encoder_t bSnk_encoder
  , char* arSrc_path, size_t arSrc_bytesize, decoder_t arSrc_decoder
  , char* rSnk_path, size_t rSnk_bytesize, encoder_t rSnk_encoder );
extern axi4_port_fifo_desc_t* aub_fifo_OpenAsSlave
  ( char* awSnk_path, size_t awSnk_bytesize, encoder_t awSnk_encoder
  , char* wSnk_path, size_t wSnk_bytesize, encoder_t wSnk_encoder
  , char* bSrc_path, size_t bSrc_bytesize, decoder_t bSrc_decoder
  , char* arSnk_path, size_t arSnk_bytesize, encoder_t arSnk_encoder
  , char* rSrc_path, size_t rSrc_bytesize, decoder_t rSrc_decoder );
extern void aub_fifo_Close (axi4_port_fifo_desc_t* desc);

#ifdef __cplusplus
}
#endif

#endif
