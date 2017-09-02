
include flags.mk

VPATH = src

C_OBJS = \
	kirk-c.o \
	kirk-real-obj.o \
	kirk-dyadic-real.o

CC_OBJS =

HEADERS = \
	include/kirk-common.h \
	include/kirk-c-types.h \
	include/kirk-real-obj.h \
	include/kirk-dyadic-real.h

LIB = kirk

TESTS =

all: libkirk.so

libkirk.so:
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)
