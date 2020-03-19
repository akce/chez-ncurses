# chez-ncurses Makefile.
# Written by Akce 2019-2020.
# SPDX-License-Identifier: Unlicense

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
DEST = ~/lib

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

# SOBJS need to be in order of dependencies first, library last so that they can be joined for the distribution file.
SOBJS = ncurses.chezscheme.so

all: $(SOBJS)

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install: all
	$(INSTALL) -D -t $(DEST) $(SOBJS)

clean:
	rm -f $(SOBJS)
