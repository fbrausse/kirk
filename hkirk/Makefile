
include ../config.mk

# enforce linkage to locally built libraries for tests
PKG_DB = dist/package.conf.inplace

.PHONY: all config clean

all: $(PKG_DB)

$(PKG_DB): dist/cabal-config-flags
	cabal build

config: dist/cabal-config-flags

dist/cabal-config-flags:
	cabal configure \
		--extra-lib-dirs ../kirk --extra-include-dirs ../kirk/include || { cabal clean; exit 1; }
#		--extra-lib-dirs $(DESTDIR)/lib --extra-include-dirs $(DESTDIR)/include \

clean:
	cabal clean

include ../kirk/flags.mk

HSLDFLAGS += $(HSLD_RPATH_PREFIX)$(realpath ../kirk)
LDLIBS := -lkirk $(LDLIBS)

