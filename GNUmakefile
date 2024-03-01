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

# Scheme compilation is handled by building TOPSRC (for local builds) or ITOPSRC (for
# the install directory) and letting Chez scheme automatically manage dependents.

PROJ = ncurses

# Top level (ie, root) library sources.
# These are the scheme libs that are included from client apps.
TOPSRC = $(PROJ).chezscheme.sls $(addprefix $(PROJ)/,panel.chezscheme.sls posix.chezscheme.sls util.sls)

# Source files, shared objects, and whole program optimisations for the library subdirectory.
SUBSRC = $(addprefix $(PROJ)/,common.chezscheme.sls)

# Setup shared object and whole program optimisation extensions.
TOPOBJ = $(TOPSRC:.sls=.so)
TOPWPO = $(TOPSRC:.sls=.wpo)
SUBOBJ = $(SUBSRC:.sls=.so)
SUBWPO = $(SUBSRC:.sls=.wpo)

# Local build versions.
BTOPOBJ = $(addprefix $(BUILDDIR)/,$(TOPOBJ))
BTOPWPO = $(addprefix $(BUILDDIR)/,$(TOPWPO))
BSUBOBJ = $(addprefix $(BUILDDIR)/,$(SUBOBJ))
BSUBWPO = $(addprefix $(BUILDDIR)/,$(SUBWPO))

# Installed versions of all the above.
ITOPSRC = $(addprefix $(LIBDIR)/,$(TOPSRC))
ITOPOBJ = $(addprefix $(LIBDIR)/,$(TOPOBJ))
ITOPWPO = $(addprefix $(LIBDIR)/,$(TOPWPO))
ISUBSRC = $(addprefix $(LIBDIR)/,$(SUBSRC))
ISUBOBJ = $(addprefix $(LIBDIR)/,$(SUBOBJ))
ISUBWPO = $(addprefix $(LIBDIR)/,$(SUBWPO))

# Tell GNU make about the files generated as a "side-effect" of building TOPWPO,
# otherwise make will raise an error that it doesn't know how to build these.
.SECONDARY: $(TOPOBJ)

# Default to just a local build.
all: build

# In-place local development test compile. This is built in a separate
# directory BUILDDIR so as to keep build files out of the way.
# $(basename $*) extracts the useful library name.
# eg, (import (proj $(basename proj/libname.chezscheme.sls))) => (import (proj libname))

$(BUILDDIR)/$(PROJ)/%.wpo: $(PROJ)/%.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(BUILDDIR)")))'	\
		'(import ($(PROJ) $(basename $*)))'	\
		| $(SCHEME) $(SFLAGS)

$(BUILDDIR)/%.wpo: %.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(BUILDDIR)")))'	\
		'(import ($(basename $*)))'		\
		| $(SCHEME) $(SFLAGS)

# Installed compile. Source files must be copied to destination LIBDIR first
# (via make rules) where this recipe compiles in the remote location.

$(LIBDIR)/$(PROJ)/%.wpo: $(PROJ)/%.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(LIBDIR)")))'	\
		'(import ($(PROJ) $(basename $*)))'	\
		| $(SCHEME) $(SFLAGS)

$(LIBDIR)/%.wpo: %.sls
	echo	\
		"(reset-handler abort)"			\
		"(compile-imported-libraries #t)"	\
		"(generate-wpo-files #t)"		\
		"(library-directories"			\
		'  (list (cons "." "$(LIBDIR)")))'	\
		'(import ($(basename $*)))'		\
		| $(SCHEME) $(SFLAGS)

# Creating the dest dir first works for both BSD and GNU install.

$(LIBDIR)/%: %
	@$(INSTALL) -d "$(dir $@)"
	$(INSTALL) -m u=rw,go=r,a-s -p "$<" "$@"

build: $(BTOPWPO)

install: install-build

install-build: install-src $(ITOPWPO)

install-src: $(ITOPSRC) $(ISUBSRC)

clean:
	$(RM) $(BTOPOBJ) $(BTOPWPO) $(BSUBOBJ) $(BSUBWPO)
	$(RM) -d $(BUILDDIR)/$(PROJ)
	$(RM) -d $(BUILDDIR)

clean-install:
	$(RM) $(ITOPOBJ) $(ITOPWPO) $(ITOPSRC) $(ISUBSRC) $(ISUBOBJ) $(ISUBWPO)
	$(RM) -d $(LIBDIR)/$(PROJ)

clean-all: clean clean-install
