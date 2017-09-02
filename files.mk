
C_OBJS = \
	kirk-c.o \
	kirk-real-obj.o \
	kirk-dyadic-real.o \
	kirk-hs.o \
	test-irram.o

CC_OBJS = \
	kirk-iRRAM.o \
	kirk-irram-api.o \
	logmap-irram.o

HS_OBJS = \
	Data/Number/Kirk.o \
	Data/Number/Kirk/Debug.o \
	Data/Number/Kirk/Irram.o \
	test-hs.o \
	logmap.o

HS_HEADERS = $(HS_OBJS:.o=.hi)

TESTS = \
	test-irram \
	test-hs \
	logmap

LIB_OBJS = \
	kirk-c.o \
	kirk-real-obj.o \
	kirk-dyadic-real.o

LIB_HEADERS = \
	kirk-common.h \
	kirk-c-types.h \
	kirk-real-obj.h \
	kirk-dyadic-real.h

LIB_IRRAM_OBJS = \
	kirk-iRRAM.o \
	kirk-irram-api.o

LIB_IRRAM_HEADERS = \
	kirk-iRRAM.hh \
	kirk-irram-api.h

LIB_HS_OBJS = \
	kirk-hs.o \
	Data/Number/Kirk.o \
	Data/Number/Kirk/Debug.o

LIB_HS_HEADERS = \
	Data/Number/Kirk.hi \
	Data/Number/Kirk/Debug.hi

LIBS = \
	libkirk.a \
	libkirk-irram.a \
	libkirk-hs.a
