# Chez ncurses bindings

chez-ncurses: [Chez Scheme] bindings for [ncurses].

## Compiling and installing

The recommended install method is to use [GNU make](https://www.gnu.org/software/make/).

To compile and install the library shared-object to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib/csv&lt;CHEZ-SCHEME-VERSION&gt;). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in (library-directories). One way to add is by setting CHEZSCHEMELIBDIRS.

The default install target will install source files, shared object files, and [whole program optimisation](https://cisco.github.io/ChezScheme/csug9.5/system.html#./system:s117) files to LIBDIR. Other targets exist that install source only (install-src) and objects only (install-so).

## Dependancies

[ncurses] wide char version. eg, **libncursesw.so**.

[ncurses] is an optional dependancy of [Chez scheme] so chances are good that it's already installed on your system. If not, you'll need to install manually.

## How To Use

```scheme
(import (ncurses))
```
The bindings are very thin wrappers around the [ncurses] C interface, however there are a few differences.

C Function names that contain an underscore are converted to lisp-case/kebab-case.

eg, start_color() -> (start-color)

[ncurses] constants retain their CAPITALISED_UNDERSCORE format because the author finds it easier to keep the SHIFT key held down while typing these in.

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

Function char returns are unchanged (perhaps only for now?) so C [ncurses] functions that return a `char` are still returned as an `int` in scheme.

eg, (integer? (getch)) => #t

All objects that are variables in the C [ncurses] interface should be variables in this interface, including volatile ones such as `stdscr`, `COLS`, `ROWS` etc. This is achieved via [R6RS] [identifier-syntax](http://scheme.com/tspl4/syntax.html#./syntax:s27) which hides the underlying [ffi](https://cisco.github.io/ChezScheme/csug9.5/foreign.html#./foreign:h0) accessor calls.

[getch](https://www.invisible-island.net/ncurses/man/curs_getch.3x.html) and similar in the scheme bindings are bound to their wide-char counterparts. eg, [get_wch](https://www.invisible-island.net/ncurses/man/curs_get_wch.3x.html).

As [R6RS] and [Chez Scheme] encode chars using unicode, i'm not sure that there's any reason to use plain `getch`.

Some of the native [ncurses] mouse functions require memory references to get values, which for Chez Scheme means managing foreign memory. These functions have been changed to make things simpler for client code. Namely:

C signature | Scheme signature
----------- | ----------------
mousemask(mmask_t new_mask, mmask_t* old_mask) => actual_mask | (mousemask new_mask) => (values actual-mask old-mask)
getmouse(MEVENT* mouse_event) | (call-with-mouse-event procedure) => return value of (procedure mevent*) or ERR
mouse_trafo(int* y, int* x, bool to_screen) | (wmouse-trafo y x to-screen?) => (values y x) or error exception
wmouse_trafo(WINDOW* win, int* y, int* x, bool to_screen) | (wmouse-trafo win* y x to-screen?) => (values y x) or error exception

`call-with-mouse-event` is a special case in that it's not an [ncurses] function, but a useful wrapper for `getmouse`. `call-with-mouse-event` manages the foreign memory used to store the mouse event structure and only calls `procedure` if mouse event retrieval was successful. Note that the mouse event memory reference is only valid while `procedure` is running, so using `ungetmouse` should only be done from within that context.

Accessors for mevent* members are provided: `mevent-id`, `mevent-y`, `mevent-x`, `mevent-z` and `mevent-bstate`.

Apart from those differences, it should be possible to learn how to use these bindings via the [ncurses] man pages and by studying the included [example](example.ss) program.

## setlocale

As a convenience, **chez-ncurses** includes a binding for [setlocale(3)](https://www.man7.org/linux/man-pages/man3/setlocale.3.html) as it's near mandatory to call this before initialising [ncurses].

Ideally, this belongs in a separate POSIX style library.

## TODO

- [x] Add support for the mouse. ie, things in [curs_mouse(3)](https://invisible-island.net/ncurses/man/curs_mouse.3x.html)
- [x] ~~Document and add an~~ example that uses ALT keystrokes
- [ ] Create a separate POSIX [Chez scheme] library and move **setlocale** there
- [ ] Include example and doc on how to use with an event lib like [chez-libev](https://github.com/akce/chez-libev)

## Links

[Chez Scheme](https://cisco.github.io/ChezScheme/)

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"

[ncurses](https://invisible-island.net/ncurses/)

[ncurses]: https://invisible-island.net/ncurses/ "ncurses"

[R6RS](http://r6rs.org/)

[R6RS]: http://r6rs.org "R6RS"

## License

chez-ncurses is an [unlicensed](LICENSE) work released into the Public Domain.
