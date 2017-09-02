
include flags.mk

#  all    : libkirk-irram.a
#  install: libkirk-irram.a
#  tests  : test-irram
#  test-irram: LDFLAGS += -pthread -L$(IRRAM)/lib -Wl,-rpath,$(IRRAM)/lib
#  test-irram: LDLIBS  += -liRRAM

VPATH = src tests

CC_OBJS = \
	kirk-irram.o \
	kirk-irram-api.o

HEADERS = \
	include/kirk-irram.hh \
	include/kirk-irram-api.h

TESTS = \
	test-irram

LIB = kirk-irram

all:
tests: test-irram

test-irram: tests/test-irram.c | libkirk-irram.so
test-irram: LDFLAGS += -L. -Wl,-rpath,.
test-irram: LDLIBS := -lkirk-irram $(LDLIBS)

libkirk-irram.so:
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)
