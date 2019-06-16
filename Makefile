# chez-ncurses Makefile.
# Placed in the public domain.

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

COBJS =

# SOBJS need to be in order of dependencies first, library last so that they can be joined for the distribution file.
SOBJS = ncurses.so

BINS = ncurses.chezscheme.so

all: $(BINS)

ncurses.chezscheme.so: $(SOBJS)
	cat $^ > $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

%.so: %.ss
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install: all
	$(INSTALL) -D -t $(DEST) $(BINS)

clean:
	rm -f $(COBJS) $(SOBJS) $(BINS)
