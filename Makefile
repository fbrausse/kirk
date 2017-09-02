
# commands
INSTALL       = install
HSC           = ghc
HS_PKGS_CMD   = $(HSC)-pkg --simple-output list
HS_LIBDIR     = $(shell $(HSC) --print-libdir)

# paths
DESTDIR      ?= /usr/local
IRRAM        ?= $(realpath $(HOME)/iRRAM/installed)

# flags
OPT_FLAGS     = -O2
WARN_FLAGS    = -Wall -Wextra
FLAGS         = $(OPT_FLAGS) $(WARN_FLAGS) -g

override CC  += -std=c99
override CXX += -std=c++14
CPPFLAGS     += -DKIRK_CHECK_BOUND -UKIRK_BOUND_SIZE_GMP
CFLAGS        = $(FLAGS) -pedantic
CXXFLAGS      = $(FLAGS) -pedantic
HSFLAGS       = $(FLAGS) -cpp -dynamic -fno-full-laziness -fforce-recomp
LDLIBS        = -lmpfr -lm
ARFLAGS       = rcs

include files.mk

# ----------------------------------------------------------------------
# dynamic configuration
# ----------------------------------------------------------------------

ifneq ($(IRRAM),)
  CPPFLAGS += -I$(IRRAM)/include -DKIRK_HAVE_IRRAM
  CFLAGS   += -pthread
  CXXFLAGS += -pthread
  HSFLAGS  += -threaded # allow FFI function calls from different OS-threads
  all    : libkirk-irram.a
  install: libkirk-irram.a
  tests  : test-irram
  test-irram: LDFLAGS += -pthread -L$(IRRAM)/lib -Wl,-rpath,$(IRRAM)/lib
  test-irram: LDLIBS  += -liRRAM
endif

ifeq ($(HS_MPFR),)
  ROUNDED ?= $(shell $(HS_PKGS_CMD) rounded-0.1)
  HMPFR   ?= $(shell $(HS_PKGS_CMD) hmpfr-0.4.3)
  ifneq ($(ROUNDED),)
    CPPFLAGS += -DKIRK_HAVE_ROUNDED
    HS_MPFR   = $(ROUNDED)
  else ifneq ($(HMPFR),)
    CPPFLAGS += -DKIRK_HAVE_HMPFR
    HS_MPFR   = $(HMPFR)
  endif
endif

ifneq ($(HS_MPFR),)
  CPPFLAGS += -I$(HS_LIBDIR)/include -DKIRK_HAVE_HASKELL
  all    : libkirk-hs.a
  install: libkirk-hs.a
  ifneq ($(IRRAM),)
    test-hs logmap: LDFLAGS += -L$(IRRAM)/lib -optl-Wl,-rpath,$(IRRAM)/lib
    test-hs logmap: LDLIBS  += -liRRAM -lstdc++ -package $(HS_MPFR)
    tests: test-hs logmap
  endif
endif

all: libkirk.a tests

debug: OPT_FLAGS = -O0
debug: tests

test-irram: test-irram.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

test-hs: test-hs.o
	$(HSC) $(HSFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

logmap: logmap.o logmap-irram.o
	$(HSC) $(HSFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

kirk-hs.o: Data/Number/Kirk_stub.h
Data/Number/Kirk_stub.h: Data/Number/Kirk.o
Data/Number/Kirk/Irram.o: Data/Number/Kirk.o
Data/Number/Kirk/Debug.o: Data/Number/Kirk.o

$(TESTS): libkirk.a
test-irram: libkirk-irram.a
test-hs.o logmap.o: Data/Number/Kirk/Irram.o
test-hs logmap: Data/Number/Kirk/Irram.o libkirk-hs.a libkirk-irram.a

libkirk.a: $(LIB_OBJS)
libkirk-irram.a: $(LIB_IRRAM_OBJS)
libkirk-hs.a: $(LIB_HS_OBJS)

%.a:
	$(AR) $(ARFLAGS) $@ $^

$(HS_OBJS): %.o: %.hs Makefile
	$(HSC) $(CPPFLAGS) $(HSFLAGS) -c -o $@ $<
$(CC_OBJS): %.o: %.cc $(wildcard *.h *.hh) Makefile
$(C_OBJS): %.o: %.c $(wildcard *.h) Makefile

install: libkirk.a
	mkdir -p $(DESTDIR)/include/kirk && $(INSTALL) -m 0644 -t $(DESTDIR)/include/kirk $(LIB_HEADERS)
	mkdir -p $(DESTDIR)/lib && $(INSTALL) -m 0644 -t $(DESTDIR)/lib libkirk.a

uninstall:
	$(RM) $(DESTDIR)/include/kirk/kirk-c-types.h
	$(RM) $(DESTDIR)/lib/libkirk.a

clean:
	$(RM) $(C_OBJS) $(CC_OBJS) $(HS_OBJS) $(HS_HEADERS) $(LIBS) $(TESTS) Data/Number/Kirk_stub.h

.PHONY: tests install uninstall clean
