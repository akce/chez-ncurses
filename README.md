# Chez ncurses bindings

chez-ncurses: [Chez Scheme] bindings for [ncurses].

## Compiling and installing

The recommended install method is to use the Makefile.

To compile and install the library shared-object to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in (library-directories). One way to add is by setting CHEZSCHEMELIBDIRS.

## How To Use

```scheme
(import (ncurses))
```
The bindings are very thin wrappers around the [ncurses] C interface. The main difference is that functions (*only!*) are converted to lisp-case/kebab-case.

eg, start_color() -> (start-color)

It should be possible to learn how to use these bindings via the ncurses man pages.

A simple example program is included.

## Links

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[ncurses]: https://invisible-island.net/ncurses/ "ncurses"

## License

chez-ncurses is an Unlicensed work released into the Public Domain.
