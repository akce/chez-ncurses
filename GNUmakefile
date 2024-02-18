# chez-ncurses GNUmakefile.
# Written by Jerry 2019-2021.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME = chez-scheme

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR = ~/lib/csv$(shell $(SCHEME) --version 2>&1)

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

# This makefile assumes a library layout as follows:
# TOP
# PROJDIR/
#   FFI
#   SUBSRC ..
#   SUBOBJ ..
#   SUBWPO ..
#
# Where TOP is the high level library definition that imports all sub libs within PROJDIR.
# FFI (if needed) is a C compilable lib.
# The rest are scheme libraries.
# Scheme compilation is handled by building TOP and letting Chez scheme automatically manage dependants.

PROJDIR = ncurses

# Top level (ie, root) library source, .. etc.
TOPSRC = ncurses.chezscheme.sls
TOPOBJ = $(TOPSRC:.sls=.so)
TOPWPO = $(TOPSRC:.sls=.wpo)

# Installed versions of all the above.
ITOPSRC = $(addprefix $(LIBDIR)/,$(TOPSRC))
ITOPOBJ = $(addprefix $(LIBDIR)/,$(TOPOBJ))
ITOPWPO = $(addprefix $(LIBDIR)/,$(TOPWPO))

# Tell GNU make about the files generated as a "side-effect" of building TOPWPO,
# otherwise make will raise an error that it doesn't know how to build these.
.SECONDARY: $(TOPOBJ)

# Default to just a local build.
all: build

# Build target is structured so that the main wpo file is dependant on all scheme source files and triggers
# a Chez compile such that Chez rebuilds all dependancies on demand.
$(TOPWPO): $(TOPSRC)
	echo '(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories ".") (compile-library "$(TOPSRC)")' | $(SCHEME) $(SFLAGS)

$(LIBDIR)/%: %
	$(INSTALL) -pv "$<" $(LIBDIR)

build: $(TOPWPO)

# Default install target is for everything.
install: install-so install-src

install-so: $(ITOPWPO) $(ITOPOBJ)

install-src: $(ITOPSRC)

clean:
	$(RM) $(TOPOBJ) $(TOPWPO)

clean-install:
	$(RM) $(ITOPOBJ) $(ITOPWPO) $(ITOPSRC)

clean-all: clean clean-install
