
include ../kirk/flags.mk

IRRAM    ?= $(realpath $(HOME)/iRRAM/installed)
CPPFLAGS += -I$(IRRAM)/include -DKIRK_HAVE_IRRAM -I../kirk-irram/include
CFLAGS   += -pthread
CXXFLAGS += -pthread
HSFLAGS  += -threaded # allow FFI function calls from different OS-threads
LDLIBS   := -liRRAM -lkirk $(LDLIBS)
LDFLAGS  += -pthread -L../kirk -Wl,-rpath,../kirk -L$(IRRAM)/lib -Wl,-rpath,$(IRRAM)/lib
