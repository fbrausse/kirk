
include ../config.mk

TESTS = \
	test-hs \
	logmap

# enforce linkage to locally built libraries for tests
PKG_DB = dist/package.conf.inplace
PKG_CONF = $(wildcard $(PKG_DB)/hkirk-irram-*.conf)
# echo -e '#include "dist/build/autogen/cabal_macros.h"\nCURRENT_PACKAGE_KEY' | $(CPP) | grep -v '^\s*#' | tr -d  '"'
PKG_ID = $(patsubst %.conf,%,$(notdir $(PKG_CONF)))

.PHONY: all clean config tests

all: $(PKG_DB) tests

$(PKG_DB): dist/cabal-config-flags
	cabal build

tests: $(PKG_DB)
	$(MAKE) $(TESTS)

config: dist/cabal-config-flags

dist/cabal-config-flags: hkirk-irram.cabal
	cabal configure --ghc-pkg-options="--force" \
		--extra-lib-dirs $(IRRAM)/lib --extra-include-dirs $(IRRAM)/include \
		--extra-lib-dirs ../kirk-irram --extra-include-dirs ../kirk-irram/include \
		--package-db ../hkirk/$(PKG_DB) || { cabal clean; exit 1; }
#		--extra-lib-dirs $(DESTDIR)/lib --extra-include-dirs $(DESTDIR)/include \

include ../kirk-irram/flags.mk

HSLDFLAGS += $(HSLD_RPATH_PREFIX)$(realpath ../kirk-irram)
LDLIBS := -lkirk-irram -lstdc++ $(LDLIBS)

$(TESTS): %: tests/%.hs
	$(HSC) $(HSFLAGS) $(HSLDFLAGS) -o $@ $^ $(LDLIBS) -package-db ../hkirk/$(PKG_DB) -package-db $(PKG_DB) -package-id $(PKG_ID)

logmap: logmap-irram.o

logmap-irram.o: tests/logmap-irram.cc Makefile
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

clean:
	cabal clean
	$(RM) $(TESTS) logmap-irram.o
