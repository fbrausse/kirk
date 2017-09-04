
include ../kirk/flags.mk

IRRAM     ?= $(realpath $(HOME)/iRRAM/installed)
CPPFLAGS  += -I$(IRRAM)/include -DKIRK_HAVE_IRRAM -I../kirk-irram/include
CFLAGS    += -pthread
CXXFLAGS  += -pthread
LDLIBS    := -liRRAM -lkirk $(LDLIBS)
LDFLAGS   += -pthread -L../kirk $(CCLD_RPATH_PREFIX)$(realpath ../kirk) -L$(IRRAM)/lib $(CCLD_RPATH_PREFIX)$(IRRAM)/lib
HSLDFLAGS += -threaded # allow FFI function calls from different OS-threads
HSLDFLAGS += -L../kirk $(HSLD_RPATH_PREFIX)$(realpath ../kirk) -L$(IRRAM)/lib $(HSLD_RPATH_PREFIX)$(IRRAM)/lib
