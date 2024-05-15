BSC = bsc
CC ?= gcc

BLUESTUFFDIR ?= ../../..
include $(BLUESTUFFDIR)/bluestuff.inc.mk
BSVPATH = +:$(BLUESTUFF_DIRS)

# generated files directories
BUILDDIR = build
BDIR = $(BUILDDIR)/bdir
SIMDIR = $(BUILDDIR)/simdir

BSCFLAGS = -p $(BSVPATH)
BSCFLAGS += -bdir $(BDIR)
BSCFLAGS += -simdir $(SIMDIR)
BSCFLAGS += -suppress-warnings T0127:S0080 # no orphan typeclass warning

all: testReadsWideToNarrow testReadsNarrowToWide testWritesWideToNarrow testWritesNarrowToWide

$(SIMDIR):
	mkdir -p $(BDIR) $(SIMDIR)

testReadsWideToNarrow: AXI4WidthsExample.bsv $(SIMDIR) $(SIMDIR)/MemSim.o
	$(BSC) $(BSCFLAGS) -check-assert -sim -g testReadsWideToNarrow -u $<
	$(BSC) $(BSCFLAGS) -check-assert -sim -e testReadsWideToNarrow -Xl $(SIMDIR)/MemSim.o -o $@

testReadsNarrowToWide: AXI4WidthsExample.bsv $(SIMDIR) $(SIMDIR)/MemSim.o
	$(BSC) $(BSCFLAGS) -check-assert -sim -g testReadsNarrowToWide -u $<
	$(BSC) $(BSCFLAGS) -check-assert -sim -e testReadsNarrowToWide -Xl $(SIMDIR)/MemSim.o -o $@

testWritesWideToNarrow: AXI4WidthsExample.bsv $(SIMDIR) $(SIMDIR)/MemSim.o
	$(BSC) $(BSCFLAGS) -check-assert -sim -g testWritesWideToNarrow -u $<
	$(BSC) $(BSCFLAGS) -check-assert -sim -e testWritesWideToNarrow -Xl $(SIMDIR)/MemSim.o -o $@

testWritesNarrowToWide: AXI4WidthsExample.bsv $(SIMDIR) $(SIMDIR)/MemSim.o
	$(BSC) $(BSCFLAGS) -check-assert -sim -g testWritesNarrowToWide -u $<
	$(BSC) $(BSCFLAGS) -check-assert -sim -e testWritesNarrowToWide -Xl $(SIMDIR)/MemSim.o -o $@

$(SIMDIR)/MemSim.o: $(BLUEUTILSDIR)/MemSim.c
	$(CC) $(CCFLAGS) -c -o $@ $<

CCFLAGS = -O3  -Wall -Wno-unused -D_FILE_OFFSET_BITS=64 -fPIC

.PHONY: clean

clean:
	rm -f testReadsWideToNarrow*
	rm -f testReadsNarrowToWide*
	rm -f testWritesWideToNarrow*
	rm -f testWritesNarrowToWide*
	rm -rf $(BUILDDIR)
