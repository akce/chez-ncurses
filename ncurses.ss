(library (ncurses)
  (export
   ERR OK

   ;; curs_variables (expressed as functions).
   COLORS COLOR_PAIRS COLS ESCDELAY LINES TABSIZE curscr newscr stdscr

   ;; Colour #defines.
   COLOR_BLACK COLOR_RED COLOR_GREEN COLOR_YELLOW COLOR_BLUE COLOR_MAGENTA COLOR_CYAN COLOR_WHITE

   ;; curs_initscr
   initscr endwin

   ;; curs_window
   newwin delwin mvwin subwin derwin mvderwin dupwin wsyncup syncok wcursyncup wsyncdown

   ;; curs_refresh
   refresh wrefresh wnoutrefresh doupdate redrawwin wredrawln

   ;; curs_clear
   erase werase clear wclear clrtobot wclrtobot clrtoeol wclrtoeol

   ;; curs_addch
   addch waddch mvaddch mvwaddch echochar wechochar

   ;; curs_addstr
   addstr addnstr waddstr waddnstr mvaddstr mvaddnstr mvwaddstr mvwaddnstr

   ;; curs_border
   border wborder box hline whline vline wvline mvhline mvwhline mvvline mvwvline

   ;; curs_inopts
   cbreak nocbreak echo noecho halfdelay intrflush keypad meta nodelay raw noraw

   ;; curs_color
   start-color has-colors can-change_color init-pair init-color color-content pair-content
   init-extended-pair init-extended-color extended-color-content extended-pair-content reset-color-pairs

   ;; curs_attr
   attr-get wattr-get attr-set wattr-set attr-off wattr-off attr-on wattr-on attroff wattroff attron wattron attrset wattrset chgat wchgat mvchgat mvwchgat color-set wcolor-set standend wstandend standout wstandout

   ;; curs_bkgd
   bkgdset wbkgdset bkgd wbkgd getbkgd

   ;; curs_beep
   beep flash

   ;; resizeterm
   is-term-resized resize-term resizeterm

   ;; curs_kernel
   def-prog-mode def-shell-mode reset-prog-mode reset-shell-mode resetty savetty ripoffline curs-set napms

   ;; default_colors
   use-default-colors assume-default-colors)
  (import
   (except (chezscheme) box meta))

  (define library-init
    (load-shared-object "libncursesw.so.6"))

  (define-ftype window* void*)
  (define-ftype chtype unsigned)
  (define-ftype attr_t chtype)

  (define-syntax c/vars
    (lambda (stx)
      (syntax-case stx ()
        [(_ var type)
         #`(define var
             (let ([addr (foreign-entry #,(symbol->string (syntax->datum #'var)))])
               (lambda ()
                 (foreign-ref type addr 0))))]
        [(_ (n t) ...)
         #'(begin
             (c/vars n t) ...)])))

  (define-syntax enum
    (syntax-rules ()
      [(_ (col val) ...)
       (begin
         (define col val) ...)]))

  (enum
   (ERR	-1)
   (OK	0))

  (c/vars
   (COLORS	'int)
   (COLOR_PAIRS	'int)
   (COLS	'int)
   (ESCDELAY	'int)
   (LINES	'int)
   (TABSIZE	'int)
   (curscr	'void*)
   (newscr	'void*)
   (stdscr	'void*))

  (enum
   (COLOR_BLACK		0)
   (COLOR_RED		1)
   (COLOR_GREEN		2)
   (COLOR_YELLOW	3)
   (COLOR_BLUE		4)
   (COLOR_MAGENTA	5)
   (COLOR_CYAN		6)
   (COLOR_WHITE		7))

  (define-syntax c_funcs
    (lambda (stx)
      (define string-map
        (lambda (func str)
          (list->string (map func (string->list str)))))
      (define symbol->curses-name
        (lambda (sym)
          (string-map (lambda (c)
                        (if (eqv? c #\-)
                            #\_ c))
                      (symbol->string sym))))
      (syntax-case stx ()
        [(_ (name args return))
         (quasisyntax
          (define name
            (foreign-procedure (unsyntax (symbol->curses-name (syntax->datum #'name))) args return)))]
        [(_ f ...) (syntax (begin (c_funcs f) ...))])))

  (c_funcs
   ;; curs_initscr
   (initscr () window*)
   (endwin () int)

   ;; curs_window
   (newwin (int int int int) window*)
   (delwin (window*) int)
   (mvwin (window* int int) int)
   (subwin (window* int int int int) window*)
   (derwin (window* int int int int) window*)
   (mvderwin (window* int int) int)
   (dupwin (window*) window*)
   (wsyncup (window*) void)
   (syncok (window* boolean) int)
   (wcursyncup (window*) void)
   (wsyncdown (window*) void)

   ;; curs_refresh
   (refresh () int)
   (wrefresh (window*) int)
   (wnoutrefresh (window*) int)
   (doupdate () int)
   (redrawwin (window*) int)
   (wredrawln (window* int int) int)

   ;; curs_clear
   (erase () int)
   (werase (window*) int)
   (clear () int)
   (wclear (window*) int)
   (clrtobot () int)
   (wclrtobot (window*) int)
   (clrtoeol () int)
   (wclrtoeol (window*) int)

   ;; curs_addch
   (addch (chtype) int)
   (waddch (window* chtype) int)
   (mvaddch (int int chtype) int)
   (mvwaddch (window* int int chtype) int)
   (echochar (chtype) int)
   (wechochar (window* chtype) int)

   ;; curs_addstr
   (addstr (string) int)
   (addnstr (string int) int)
   (waddstr (window* string) int)
   (waddnstr (window* string int) int)
   (mvaddstr (int int string) int)
   (mvaddnstr (int int string int) int)
   (mvwaddstr (window* int int string) int)
   (mvwaddnstr (window* int int string int) int)

   ;; curs_border
   (border (chtype chtype chtype chtype chtype chtype chtype chtype) int)
   (wborder (window* chtype chtype chtype chtype chtype chtype chtype chtype) int)
   (box (window* chtype chtype) int)
   (hline (chtype int) int)
   (whline (window* chtype int) int)
   (vline (chtype int) int)
   (wvline (window* chtype int) int)
   (mvhline (int int chtype int) int)
   (mvwhline (window* int int chtype int) int)
   (mvvline (int int chtype int) int)
   (mvwvline (window* int int chtype int) int)

   ;; curs_inopts
   (cbreak() int)
   (nocbreak() int)
   (echo () int)
   (noecho () int)
   (halfdelay (int) int)
   (intrflush (window* boolean) int)
   (keypad (window* boolean) int)
   (meta (window* boolean) int)
   (nodelay (window* boolean) int)
   (raw () int)
   (noraw () int)

   ;; curs_color
   (start-color () int)
   (has-colors () boolean)
   (can-change_color () boolean)
   (init-pair (short short short) int)
   (init-color (short short short short) int)
   (color-content (short (* short) (* short) (* short)) int)
   (pair-content (short (* short) (* short)) int)
   ;; extensions
   (init-extended-pair (int int int) int)
   (init-extended-color (int int int int) int)
   (extended-color-content (short (* short) (* short) (* short)) int)
   (extended-pair-content (short (* short) (* short)) int)
   (reset-color-pairs () void)

   ;; curs_attr
   (attr-get ((* attr_t) (* short) void*) int)
   (wattr-get (window* (* attr_t) (* short) void*) int)
   (attr-set (attr_t short void*) int)
   (wattr-set (window* attr_t short void*) int)
   (attr-off (attr_t void*) int)
   (wattr-off (window* attr_t void*) int)
   (attr-on (attr_t void*) int)
   (wattr-on (window* attr_t void*) int)
   (attroff (int) int)
   (wattroff (window* int) int)
   (attron (int) int)
   (wattron (window* int) int)
   (attrset (int) int)
   (wattrset (window* int) int)
   (chgat (int attr_t short void*) int)
   (wchgat (window* int attr_t short void*) int)
   (mvchgat (int int int attr_t short void*) int)
   (mvwchgat (window* int int int attr_t short void*) int)
   (color-set (short void*) int)
   (wcolor-set (window* short void*) int)
   (standend () int)
   (wstandend (window*) int)
   (standout () int)
   (wstandout (window*) int)

   ;; curs_bkgd
   (bkgdset (chtype) void)
   (wbkgdset (window* chtype) void)
   (bkgd (chtype) int)
   (wbkgd (window* chtype) int)
   (getbkgd (window*) chtype)

   ;; curs_beep
   (beep () int)
   (flash () int)

   ;; resizeterm
   (is-term-resized (int int) boolean)
   (resize-term (int int) int)
   (resizeterm (int int) int)

   ;; curs_kernel
   (def-prog-mode () int)
   (def-shell-mode () int)
   (reset-prog-mode () int)
   (reset-shell-mode () int)
   (resetty () int)
   (savetty () int)
   #;(getsyx (int int) void)
   #;(setsyx (int int) void)
   ;; int ripoffline(int line, int (*init)(WINDOW *, int));
   (ripoffline (int void*) int)
   (curs-set (int) int)
   (napms (int) int)

   ;; default_colors
   (use-default-colors () int)
   (assume-default-colors (int int) int)))
