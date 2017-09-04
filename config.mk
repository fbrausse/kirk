
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

override CC  += -std=c11
override CXX += -std=c++14
CFLAGS        = $(FLAGS) -pedantic
CXXFLAGS      = $(FLAGS) -pedantic
HSFLAGS       = $(FLAGS) -cpp -dynamic -fno-full-laziness -fforce-recomp
ARFLAGS       = rcs

# helpers
CCLD_RPATH_PREFIX = -Wl,-rpath,
HSLD_RPATH_PREFIX = -optl-Wl,-rpath,
