# chez-ncurses GNUmakefile.
# Written by Jerry 2019-2024.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
OS = $(shell uname)
ifeq ($(OS),FreeBSD)
	# FreeBSD pkg installs to /usr/local
	SCHEME = /usr/local/bin/chez-scheme
else
	SCHEME = /usr/bin/chez-scheme
endif
SCHEMEVERSION = $(shell $(SCHEME) --version 2>&1)

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
PREFIX = $(HOME)
LIBDIR = $(PREFIX)/lib/csv$(SCHEMEVERSION)
BUILDDIR = BUILD-csv$(SCHEMEVERSION)

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
# BUILDDIR/
#   FFIOBJ ..
#   BSUBOBJ ..
#   BSUBWPO ..
#   BTOPOBJ
#   BTOPWPO
#
# Where TOP is the high level library definition that imports all sub libs within PROJDIR.
# FFI (if needed) is a C compilable lib.
# The rest are scheme libraries.
# Scheme compilation is handled by building TOP and letting Chez scheme automatically manage dependants.

PROJDIR = ncurses

# Source files, shared objects, and whole program optimisations for the library subdirectory.
SUBSRC = $(addprefix $(PROJDIR)/,common.chezscheme.sls panel.chezscheme.sls util.sls)
SUBOBJ = $(SUBSRC:.sls=.so)
SUBWPO = $(SUBSRC:.sls=.wpo)

# Top level (ie, root) library source, .. etc.
TOPSRC = ncurses.chezscheme.sls
TOPOBJ = $(TOPSRC:.sls=.so)
TOPWPO = $(TOPSRC:.sls=.wpo)

# Built versions of scheme code above.
BSUBOBJ = $(addprefix $(BUILDDIR)/,$(SUBOBJ))
BSUBWPO = $(BUILDDIR)/$(PROJDIR)/panel.chezscheme.wpo
BTOPOBJ = $(addprefix $(BUILDDIR)/,$(TOPOBJ))
BTOPWPO = $(addprefix $(BUILDDIR)/,$(TOPWPO))

# Installed versions of all the above.
ISUBSRC = $(addprefix $(LIBDIR)/,$(SUBSRC))
ISUBOBJ = $(addprefix $(LIBDIR)/,$(SUBOBJ))
ISUBWPO = $(addprefix $(LIBDIR)/,$(SUBWPO))
ITOPSRC = $(addprefix $(LIBDIR)/,$(TOPSRC))
ITOPOBJ = $(addprefix $(LIBDIR)/,$(TOPOBJ))
ITOPWPO = $(addprefix $(LIBDIR)/,$(TOPWPO))

# Tell GNU make about the files generated as a "side-effect" of building TOPWPO,
# otherwise make will raise an error that it doesn't know how to build these.
.SECONDARY: $(TOPOBJ)

# Default to just a local build.
all: build

# In-place local development test compile. This is built in a separate
# directory BUILDDIR so as to keep build files out of the way.
$(BUILDDIR)/$(PROJDIR)/panel.chezscheme.wpo: $(PROJDIR)/panel.chezscheme.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(BUILDDIR)")))'	\
		'(import (ncurses panel))'		\
		| $(SCHEME) $(SFLAGS)

$(BUILDDIR)/%.wpo: %.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(BUILDDIR)")))'	\
		'(import ($(PROJDIR)))'			\
		| $(SCHEME) $(SFLAGS)

# Installed compile. Source files must be copied to destination LIBDIR first
# (via make rules) where this recipe compiles in the remote location.
# This rule is available but not really necessary given that apps should do
# their own whole program compilation and optimisations..
# ie, make install-src should be sufficient.
%.wpo: %.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		'(library-directories "$(LIBDIR)")'	\
		'(import ($(PROJDIR)))'			\
		| $(SCHEME) $(SFLAGS)

# Creating the dest dir first works for both BSD and GNU install.
$(LIBDIR)/%: %
	@$(INSTALL) -d "$(dir $@)"
	$(INSTALL) -m u=rw,go=r,a-s -p "$<" "$@"

build: $(BTOPWPO) $(BSUBWPO)

install: install-src

install-so: install-src $(ITOPWPO)

install-src: $(ITOPSRC) $(ISUBSRC)

clean:
	$(RM) -r $(BUILDDIR)

clean-install:
	$(RM) $(ITOPOBJ) $(ITOPWPO) $(ITOPSRC)

clean-all: clean clean-install
