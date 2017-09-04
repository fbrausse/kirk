
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
CFLAGS        = $(FLAGS) -pedantic
CXXFLAGS      = $(FLAGS) -pedantic
HSFLAGS       = $(FLAGS) -cpp -dynamic -fno-full-laziness -fforce-recomp
ARFLAGS       = rcs

# ----------------------------------------------------------------------
# dynamic configuration
# ----------------------------------------------------------------------

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
#  all    : libkirk-hs.a
#  install: libkirk-hs.a
  ifneq ($(IRRAM),)
    test-hs logmap: LDFLAGS += -L$(IRRAM)/lib -optl-Wl,-rpath,$(IRRAM)/lib
    test-hs logmap: LDLIBS  += -liRRAM -lstdc++ -package $(HS_MPFR)
#    tests: test-hs logmap
  endif
endif

shared = $(1:%.o=shared/%.o)
static = $(1:%.o=static/%.o)

SLIB = lib$(LIB).a
DLIB = lib$(LIB).so
LIBS = $(DLIB) $(SLIB)
OBJS = $(C_OBJS) $(CC_OBJS) $(HS_OBJS)
DEPS = $(patsubst %.o,%.d,$(call shared,$(OBJS)) $(call static,$(OBJS)))
$(call static,$(OBJS)): CPPFLAGS += -MMD
$(call shared,$(OBJS)): CPPFLAGS += -MMD

all: $(LIBS) tests

debug: OPT_FLAGS = -O0
debug: $(LIBS) tests

$(call shared,$(OBJS)): | $(dir $(call shared,$(OBJS)))
$(call static,$(OBJS)): | $(dir $(call static,$(OBJS)))
$(call shared,$(OBJS)): CPPFLAGS += -DPIC
$(call shared,$(OBJS)): FLAGS    += -fPIC

%/:
	mkdir -p $@

#test-irram: test-irram.o
#	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)
#
#test-hs: test-hs.o
#	$(HSC) $(HSFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)
#
#logmap: logmap.o logmap-irram.o
#	$(HSC) $(HSFLAGS) $(LDFLAGS) -o $@ $^ $(LDLIBS)

#kirk-hs.o: Data/Number/Kirk_stub.h
#Data/Number/Kirk_stub.h: Data/Number/Kirk.o
#Data/Number/Kirk/Irram.o: Data/Number/Kirk.o
#Data/Number/Kirk/Debug.o: Data/Number/Kirk.o

#$(TESTS): libkirk.a
#test-hs.o logmap.o: Data/Number/Kirk/Irram.o
#test-hs logmap: Data/Number/Kirk/Irram.o libkirk-hs.a libkirk-irram.a

#libkirk-hs.a: $(LIB_HS_OBJS)

$(DLIB): $(call shared,$(OBJS))
$(DLIB): LDFLAGS += -shared
$(SLIB): $(call static,$(OBJS))
	$(AR) $(ARFLAGS) $@ $^

$(call static,$(HS_OBJS)): static/%.o: %.hs Makefile
	$(HSC) $(CPPFLAGS) $(HSFLAGS) -c -o $@ $<
$(call static,$(CC_OBJS)): static/%.o: %.cc Makefile
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
$(call static,$(C_OBJS)): static/%.o: %.c Makefile
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(call shared,$(HS_OBJS)): shared/%.o: %.hs Makefile
	$(HSC) $(CPPFLAGS) $(HSFLAGS) -c -o $@ $<
$(call shared,$(CC_OBJS)): shared/%.o: %.cc Makefile
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<
$(call shared,$(C_OBJS)): shared/%.o: %.c Makefile
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

-include $(DEPS)

install: $(LIBS)
	mkdir -p $(DESTDIR)/include && \
		$(INSTALL) -m 0644 -t $(DESTDIR)/include $(HEADERS)
	mkdir -p $(DESTDIR)/lib && \
		$(INSTALL) -m 0755 -t $(DESTDIR)/lib $(DLIB) && \
		$(INSTALL) -m 0644 -t $(DESTDIR)/lib $(SLIB)

uninstall:
	$(RM) $(addprefix $(DESTDIR)/include/,$(HEADERS))
	$(RM) $(addprefix $(DESTDIR)/lib/,$(LIBS))

clean:
	$(RM) $(call shared,$(OBJS)) $(call static,$(OBJS)) $(HS_HEADERS) $(LIBS) $(TESTS) $(DEPS) Data/Number/Kirk_stub.h

.PHONY: tests install uninstall clean
