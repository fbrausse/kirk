
DESTDIR ?= /usr/local
INSTALL  = install

LIB_OBJS = \
	kirk-c.o

C_OBJS = \
	kirk-c.o

CC_OBJS = \
	kirk-iRRAM.o \
	test-iRRAM.o

TESTS = \
	test-iRRAM

IRRAM = $(realpath $(HOME)/iRRAM/installed)

ifneq ($(IRRAM),)
  LIB_OBJS += kirk-iRRAM.o
  CPPFLAGS += -I$(IRRAM)/include -DKIRK_HAVE_IRRAM
  CFLAGS   += -pthread
  CXXFLAGS += -pthread
  CXX       = g++-5 # unfortunately, my iRRAM branch requires this
test-iRRAM: LDFLAGS += -pthread -L$(IRRAM)/lib -Wl,-rpath,$(IRRAM)/lib
test-iRRAM: LDLIBS  += -liRRAM
all: test-iRRAM
endif

override CC  += -std=c99
override CXX += -std=c++14
CPPFLAGS     += -DKIRK_CHECK_BOUND
CFLAGS        = -O2 -Wall -Wextra -pedantic
CXXFLAGS      = -O2 -Wall -Wextra -pedantic
LDLIBS += -lmpfr

all: libkirk.a

test-iRRAM: test-iRRAM.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

$(TESTS): test-%: libkirk.a
test-%: LDFLAGS += -L.
test-%: LDLIBS  += -lkirk

libkirk.a: $(LIB_OBJS)
	$(AR) rcs $@ $^

$(CC_OBJS): %.o: %.cc $(wildcard *.h *.hh) Makefile
$(C_OBJS): %.o: %.c $(wildcard *.h) Makefile

install: libkirk.a
	mkdir -p $(DESTDIR)/include/kirk && $(INSTALL) -m 0644 kirk-c-types.h $(DESTDIR)/include/kirk
	mkdir -p $(DESTDIR)/lib && $(INSTALL) -m 0644 libkirk.a $(DESTDIR)/lib

uninstall:
	$(RM) $(DESTDIR)/include/kirk/kirk-c-types.h
	$(RM) $(DESTDIR)/lib/libkirk.a

clean:
	$(RM) $(C_OBJS) $(CC_OBJS) libkirk.a test-iRRAM

.PHONY: install uninstall clean
