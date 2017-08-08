
DESTDIR ?= /usr/local
INSTALL  = install

LIB_OBJS = \
	kirk-c.o

LIB_HEADERS = \
	kirk-common.h \
	kirk-c-types.h

C_OBJS = \
	kirk-c.o \
	test-const.o

CC_OBJS = \
	kirk-iRRAM.o \
	test-iRRAM.o

HS_OBJS = \
	Data/Number/Kirk.o \
	test-hs.o

TESTS = \
	test-iRRAM \
	test-hs

HSC = ghc

ifeq ($(HSC),ghc)
  HS_PKGS = ghc-pkg --simple-output list
endif

IRRAM = $(realpath $(HOME)/iRRAM/installed)
HMPFR = $(shell $(HS_PKGS) hmpfr-0.4.3)

ifneq ($(IRRAM),)
  LIB_OBJS    += kirk-iRRAM.o
  LIB_HEADERS += kirk-iRRAM.hh
  CPPFLAGS    += -I$(IRRAM)/include -DKIRK_HAVE_IRRAM
  CFLAGS      += -pthread
  CXXFLAGS    += -pthread
  CXX          = g++-5 # unfortunately, my iRRAM branch requires this
test-iRRAM: LDFLAGS += -pthread -L$(IRRAM)/lib -Wl,-rpath,$(IRRAM)/lib
test-iRRAM: LDLIBS  += -liRRAM
tests: test-iRRAM
endif

ifneq ($(HMPFR),)
  CPPFLAGS += -DKIRK_HAVE_HMPFR
test-hs: LDLIBS += -package $(HMPFR)
tests: test-hs
endif

override CC  += -std=c99
override CXX += -std=c++14
CPPFLAGS     += -DKIRK_CHECK_BOUND
CFLAGS        = -O2 -Wall -Wextra -pedantic
CXXFLAGS      = -O2 -Wall -Wextra -pedantic
HSFLAGS       = -O2 -Wall -Wextra -cpp
LDLIBS += -lmpfr -lm
ARFLAGS = rcs

all: libkirk.a tests

test-iRRAM: test-iRRAM.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

test-hs: test-hs.o test-const.o Data/Number/Kirk.o
	$(HSC) $(HSFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)
test-hs.o: Data/Number/Kirk.hi

$(TESTS): libkirk.a

libkirk.a: $(LIB_OBJS)
	$(AR) $(ARFLAGS) $@ $^

$(HS_OBJS): %.o: %.hs $(HS_OBJS:.o=.hi) Makefile
	$(HSC) $(CPPFLAGS) $(HSFLAGS) -c -o $@ $<
$(HS_OBJS:.o=.hi): %.hi: %.hs
$(CC_OBJS): %.o: %.cc $(wildcard *.h *.hh) Makefile
$(C_OBJS): %.o: %.c $(wildcard *.h) Makefile

install: libkirk.a
	mkdir -p $(DESTDIR)/include/kirk && $(INSTALL) -m 0644 -t $(DESTDIR)/include/kirk $(LIB_HEADERS)
	mkdir -p $(DESTDIR)/lib && $(INSTALL) -m 0644 -t $(DESTDIR)/lib libkirk.a

uninstall:
	$(RM) $(DESTDIR)/include/kirk/kirk-c-types.h
	$(RM) $(DESTDIR)/lib/libkirk.a

clean:
	$(RM) $(C_OBJS) $(CC_OBJS) $(HS_OBJS) libkirk.a $(TESTS)

.PHONY: tests install uninstall clean
