# Chez ncurses bindings

chez-ncurses: [Chez Scheme] bindings for [ncurses].

## Compiling and installing

The recommended install method is to use the Makefile.

To compile and install the library shared-object to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib/csv&lt;CHEZ-SCHEME-VERSION&gt;). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in (library-directories). One way to add is by setting CHEZSCHEMELIBDIRS.

## How To Use

```scheme
(import (ncurses))
```
The bindings are very thin wrappers around the [ncurses] C interface, however there are a few differences.

C Function names that contain an underscore are converted to lisp-case/kebab-case.

eg, start_color() -> (start-color)

Ncurses constants retain their CAPITALISED_UNDERSCORE format because the author finds it easier to keep the SHIFT key held down while typing these in.

Char, or `ch` style functions will accept either an `int` (as is the case in the C API), or a scheme char object. `int` compatibility is needed as some [ncurses] character/key constants are not convertable to unicode chars.

eg,
```scheme
(mvaddch 0 0 ACS_ULCORNER)
```
or
```scheme
(mvaddch 0 0 #\+)
```
are equally valid.

Function char returns are unchanged (perhaps only for now?) so [ncurses] functions that return a `char` are still returned as an int.

eg, (integer? (getch)) => #t

All objects that are variables in the C [ncurses] interface should be variables in this interface, including volatile ones such as `stdscr`, `COLS`, `ROWS` etc. This is achieved via R6RS `identifier-syntax` which hides the underlying `ffi` accessor calls.

Therefore, it should be possible to learn how to use these bindings via the [ncurses] man pages.

A simple example program is included.

## Links

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[ncurses]: https://invisible-island.net/ncurses/ "ncurses"

## License

chez-ncurses is an Unlicensed work released into the Public Domain.
