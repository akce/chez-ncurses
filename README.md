# Chez Scheme ncurses bindings

chez-ncurses: Basic [Chez Scheme] bindings for [ncurses].

These bindings are being written as needed, so they're incomplete but slowly evolving. Users should expect the API to change, hopefully for the better.

Currently, a lot of the core [ncurses](https://invisible-island.net/ncurses/man/ncurses.3x.html) library is here, as well as [panel(3X)](https://invisible-island.net/ncurses/man/panel.3x.html).

## News

### 2024/March

`getch` is no longer an alias for `get-wch`. Use `get-wch` for the wide character version.

Individual KEY defines have been deprecated and replaced by function accessors `key-symchar` and `key-ncint`.

All functions that return OK or ERR now check and raise `&ncurses-error` on error. The `&ncurses-error` condition is also new, and whether it's a good idea is yet to be seen..

NCurses' panel library has been added.

A new util lib containing some small helpful functions.

Window attribute functions now use extended `int` instead of `short`.

Boolean type handling has been fixed.

## Compiling and installing

The recommended install method is to use [GNU make](https://www.gnu.org/software/make/).

To compile and install the library shared-object to LIBDIR:

    $ make install

Override LIBDIR to change install location (default is ~/lib/csv&lt;CHEZ-SCHEME-VERSION&gt;). eg,

    $ make LIBDIR=/some/other/libdir install

Note that LIBDIR must be in (library-directories). One way to add is by setting CHEZSCHEMELIBDIRS.

The default install target will install source files, shared object files, and [whole program optimisation](https://cisco.github.io/ChezScheme/csug10.0/system.html#./system:s128) files to LIBDIR. It's also possible to do a source only install (via install-src).

## Dependencies

[ncurses] wide character version.

These bindings will try and load [ncurses] in this order:
- the shared library linked to the running [Chez Scheme] binary
- libncursesw.so.6
- libncurses.so.6

[ncurses] is an optional dependancy of [Chez scheme] so chances are good that it's already installed on your system. If not, you'll need to install manually.

## How To Use

```scheme
(import (ncurses))
```

[ncurses] defines `box` and `meta` functions, so you'll need to disambiguate if using `(chezscheme)` as it also defines these functions and there will be a clash.

eg,
```scheme
(import
  (chezscheme)
  (rename (ncurses) (box nc:box) (meta nc:meta)))
```

Read the [ncurses] [manpages](https://invisible-island.net/ncurses/man/ncurses.3x.html) and other [guides](https://invisible-island.net/ncurses/ncurses.faq.html#additional_reading) to learn how to use [ncurses].

Study the included [example](example.ss) program.

These bindings are very thin wrappers around the [ncurses] C interface, however there are a few differences.

A rough guide follows.

### Symbol names

C Function names that contain an underscore are converted to lisp-case/kebab-case.

eg, start_color() -> (start-color)

[ncurses] constants retain their CAPITALISED_UNDERSCORE format because i think it's easier to keep the SHIFT key held down while typing these in.

### Characters

Character functions will accept either an [ncurses] character integer or a Scheme char object. `int` compatibility is needed as some [ncurses] character/key constants are not convertable to unicode chars.

eg,
```scheme
(mvaddch 0 0 ACS_ULCORNER)
```
or
```scheme
(mvaddch 0 0 #\+)
```
will both work.

To see exactly which functions this affects, search `chtype` function defines in the bindings' source. The `c_funcs` syntax transformer generates wrapper functions that handle conversion for `chtype` function arguments only, return types remain NCurses character integers (or more correctly: unsigned integers).

ie,
```scheme
(integer? (getch)) => #t
```

Function `key-symchar` will convert an NCurses character number to either a KEY symbol like `KEY_F1` (if it's an NCURSES KEY character as defined in ncurses.h) or to a [Chez Scheme] character object.

eg, ```scheme
(key-symchar 27) => KEY_ESCAPE
(key-symchar 99) => #\c
```

`key-ncint` is provided to convert either a Scheme Character or [ncurses] KEY symbol to an NCurses character integer.

```scheme
(key-ncint 'KEY_RESIZE) => 410
```

### Special NCurses Variables

All objects that are variables in the C [ncurses] interface should be variables in this interface, including volatile ones such as `stdscr`, `COLS`, `ROWS` etc. This is achieved via [R6RS] [identifier-syntax](http://scheme.com/tspl4/syntax.html#./syntax:s27) which hides the underlying [ffi](https://cisco.github.io/ChezScheme/csug10.0/foreign.html#./foreign:h0) accessor calls.

As with [ncurses] C API, the value of these variables is undefined until after [ncurses] and its relevant subsystems have been initialised.

### Multiple values returns

Functions for which [ncurses] C API use multiple arg references to return multiple values, will use Scheme's `values` to return multiple values with errors generating an &ncurses-error exception.

These are few and generally limited to [attr_get's](https://invisible-island.net/ncurses/man/curs_attr.3x.html) and [curs_getyx(3X)](https://invisible-island.net/ncurses/man/curs_getyx.3x.html). But it's always best to check the bindings' source.

### Mouse

Some native [ncurses] mouse functions require memory references to get values, which for Chez Scheme means managing foreign memory. These functions have been changed to make things simpler for client code. Namely:

C signature | Scheme signature
----------- | ----------------
mousemask(mmask_t new_mask, mmask_t* old_mask) => actual_mask | (mousemask new_mask) => (values actual-mask old-mask)
getmouse(MEVENT* mouse_event) | (call-with-mouse-event procedure) => return value of (procedure mevent*) or ERR
mouse_trafo(int* y, int* x, bool to_screen) | (wmouse-trafo y x to-screen?) => (values y x) or error exception
wmouse_trafo(WINDOW* win, int* y, int* x, bool to_screen) | (wmouse-trafo win* y x to-screen?) => (values y x) or error exception

`call-with-mouse-event` is a special case in that it's not an [ncurses] function, but a useful wrapper for `getmouse`. `call-with-mouse-event` manages the foreign memory used to store the mouse event structure and only calls `procedure` if mouse event retrieval was successful. Note that the mouse event memory reference is only valid while `procedure` is running, so using `ungetmouse` should only be done from within that context.

Accessors for mevent* members are provided: `mevent-id`, `mevent-y`, `mevent-x`, `mevent-z` and `mevent-bstate`.

### setlocale

As a convenience, **chez-ncurses** includes a binding for [setlocale(3)](https://www.man7.org/linux/man-pages/man3/setlocale.3.html) as it's near mandatory to call this before initialising [ncurses].

Ideally, this belongs in a separate POSIX style library.

## How To Use Panels

```scheme
(import (ncurses panel))
```
These functions are implemented except for `ground-panel` and `ceiling-panel` (due to their use of a **SCREEN** type). The **usrptr** functions are also ignored, since there's probably better ways of passing data around at the scheme level.

## How To Use Utils

```scheme
(import (ncurses utils))
```
This scheme library contains functions not part of [ncurses] proper, but that i've found useful in writing my own programs.

eg, an [ncurses] app to clear the screen and wait for a keypress could be written as:
```scheme
(import
  (prefix (ncurses) nc:)
  (prefix (ncurses util) u:))
(u:text-user-interface
  (nc:clear)
  (u:key-combo-read))
```

## TODO

- [ ] Add support for [form(3X)](https://invisible-island.net/ncurses/man/form.3x.html)
- [ ] Add support for [menu(3X)](https://invisible-island.net/ncurses/man/menu.3x.html)
- [ ] Create a separate POSIX [Chez scheme] library and move **setlocale** there

## Links

[Chez Scheme](https://cisco.github.io/ChezScheme/)

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"

[ncurses](https://invisible-island.net/ncurses/)

[ncurses]: https://invisible-island.net/ncurses/ "ncurses"

[R6RS](http://r6rs.org/)

[R6RS]: http://r6rs.org "R6RS"

## License

chez-ncurses is an [unlicensed](LICENSE) work released into the Public Domain.
