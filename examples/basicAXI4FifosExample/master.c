#include <AXI4UnixBridges.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

DEF_AXI4_API(2, 32, 64, 0, 0, 0, 0, 0)

////////////////////////////////////////////////////////////////////////////////
typedef struct { fifo_desc_t* ff; int n; } arArg_t;
void* arTask (void* argPtr) {
  arArg_t* arg = (arArg_t*) argPtr;
  uint8_t arid = 0;
  uint32_t araddr = 0x10;
  uint8_t aruser = 0;
  t_axi4_arflit arflit = { .arid = &arid
                         , .araddr = (uint8_t*) &araddr
                         , .arlen = 8
                         , .arsize = 2
                         //, .arburst = AXI4_Burst_INCR
                         //, .arlock = AXI4_Lock_NORMAL
                         //, .arcache = arcache_wback_r_alloc
                         //, .arprot = AXI4_Prot (DATA,SECURE,PRIV)
                         , .arburst = 1
                         , .arlock = 2
                         , .arcache = 3
                         , .arprot = 4
                         , .arqos = 5
                         , .arregion = 6
                         , .aruser = &aruser };
  uint8_t bitBag[AXI4_AR_BYTEsz(2,32,0)];
  for (int i = 0; i < arg->n; i++) {
    araddr += i;
    while (!bub_fifo_Produce (arg->ff, (void*) &arflit));
    AXI4_AR_(2,32,0,print_flit)(&arflit);
    printf ("\n");
    AXI4_AR_(2,32,0,encode_flit)(&arflit, bitBag);
    bitHexDump(bitBag, AXI4_AR_BITsz(2,32,0));
    printf ("\n");
  }
}

////////////////////////////////////////////////////////////////////////////////
typedef struct { fifo_desc_t* ff; int n; } rArg_t;
void* rTask (void* argPtr) {
  rArg_t* arg = (rArg_t*) argPtr;
  t_axi4_rflit* rflit = AXI4_R_(2,64,0,create_flit)(NULL);
  int cnt = 0;
  while (cnt < arg->n) {
    while (!bub_fifo_Consume (arg->ff, (void*) rflit));
    AXI4_R_(2,64,0,print_flit)(rflit);
    printf ("\n");
    if (rflit->rlast) cnt++;
  }
}

////////////////////////////////////////////////////////////////////////////////
int main (int argc, char** argv) {
  if (argc < 4) {
    printf ("usage: %s PORT_DIR N_READ N_WRITE (argc: %d)\n", argv[0], argc);
    exit (EXIT_FAILURE);
  }
  size_t len = strlen (argv[1]) + 10;
  char* awSnk_path = (char*) malloc (len * sizeof(char));
  char*  wSnk_path = (char*) malloc (len * sizeof(char));
  char*  bSrc_path = (char*) malloc (len * sizeof(char));
  char* arSnk_path = (char*) malloc (len * sizeof(char));
  char*  rSrc_path = (char*) malloc (len * sizeof(char));
  strcpy (awSnk_path, argv[1]);
  strcpy ( wSnk_path, argv[1]);
  strcpy ( bSrc_path, argv[1]);
  strcpy (arSnk_path, argv[1]);
  strcpy ( rSrc_path, argv[1]);
  strcat (awSnk_path,  "/awSink");
  strcat ( wSnk_path,   "/wSink");
  strcat ( bSrc_path, "/bSource");
  strcat (arSnk_path,  "/arSink");
  strcat ( rSrc_path, "/rSource");
  axi4_port_fifo_desc_t* slvDesc = aub_fifo_OpenAsSlave
    ( awSnk_path, AXI4_AW_BYTEsz(2,32,0), &AXI4_AW_(2,32,0,encode_flit)
    ,  wSnk_path, AXI4_W_BYTEsz(64,0), &AXI4_W_(64,0,encode_flit)
    ,  bSrc_path, AXI4_B_BYTEsz(2,0), &AXI4_B_(2,0,decode_flit)
    , arSnk_path, AXI4_AR_BYTEsz(2,32,0), &AXI4_AR_(2,32,0,encode_flit)
    ,  rSrc_path, AXI4_R_BYTEsz(2,64,0), &AXI4_R_(2,64,0,decode_flit) );
  printf("opened axi4 slave port:\n");
  printf( "\taw: %s, %0d bytes (from %0d bits), encoder: %p\n"
        , awSnk_path
        , AXI4_AW_BYTEsz(2,32,0)
        , AXI4_AW_BITsz(2,32,0)
        , &AXI4_AW_(2,32,0,encode_flit));
  printf( "\tw: %s, %0d bytes (from %0d bits), encoder: %p\n"
        , wSnk_path
        , AXI4_W_BYTEsz(64,0)
        , AXI4_W_BITsz(64,0)
        , &AXI4_W_(64,0,encode_flit));
  printf( "\tb: %s, %0d bytes (from %0d bits), decoder: %p\n"
        , bSrc_path
        , AXI4_B_BYTEsz(2,0)
        , AXI4_B_BITsz(2,0)
        , &AXI4_B_(2,0,decode_flit));
  printf( "\tar: %s, %0d bytes (from %0d bits), encoder: %p\n"
        , arSnk_path
        , AXI4_AR_BYTEsz(2,32,0)
        , AXI4_AR_BITsz(2,32,0)
        , &AXI4_AR_(2,32,0,encode_flit));
  printf( "\tr: %s, %0d bytes (from %0d bits), decoder: %p\n"
        , rSrc_path
        , AXI4_R_BYTEsz(2,64,0)
        , AXI4_R_BITsz(2,64,0)
        , &AXI4_R_(2,64,0,decode_flit));
  printf("argv[1]: %s\n", argv[1]);
  printf("argv[2]: %s\n", argv[2]);
  printf("argv[3]: %s\n", argv[3]);
  printf("----------------------------\n");

  //pthread_create (&awThread, NULL, awTask, (void*) awArg);
  //pthread_create (&wThread, NULL, wTask, (void*) wArg);
  //pthread_create (&bThread, NULL, bTask, (void*) bArg);
  // ar thread
  pthread_t arThread;
  arArg_t arArg = {ff: slvDesc->ar, n: atoi(argv[2])};
  pthread_create (&arThread, NULL, arTask, (void*) &arArg);
  // r thread
  pthread_t rThread;
  rArg_t rArg = {ff: slvDesc->r, n: atoi(argv[2])};
  pthread_create (&rThread, NULL, rTask, (void*) &rArg);

  //pthread_join (awThread, NULL);
  //pthread_join (wThread, NULL);
  //pthread_join (bThread, NULL);
  pthread_join (arThread, NULL);
  pthread_join (rThread, NULL);

  aub_fifo_Close (slvDesc);

  return 0;

}
