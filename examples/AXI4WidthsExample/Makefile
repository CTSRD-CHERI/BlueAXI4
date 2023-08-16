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

all: AXI4WidthsExample

$(SIMDIR):
	mkdir -p $(BDIR) $(SIMDIR)

AXI4WidthsExample: AXI4WidthsExample.bsv $(SIMDIR) $(SIMDIR)/MemSim.o
	$(BSC) $(BSCFLAGS) -sim -g testAXI4Widths -u $<
	$(BSC) $(BSCFLAGS) -sim -e testAXI4Widths -Xl $(SIMDIR)/MemSim.o -o $@

$(SIMDIR)/MemSim.o: $(BLUEUTILSDIR)/MemSim.c
	$(CC) $(CCFLAGS) -c -o $@ $<

CCFLAGS = -O3  -Wall -Wno-unused -D_FILE_OFFSET_BITS=64 -fPIC

.PHONY: clean

clean:
	rm -f AXI4WidthsExample
	rm -f AXI4WidthsExample.bo
	rm -rf $(BUILDDIR)
