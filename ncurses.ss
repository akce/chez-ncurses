(library (ncurses)
  (export
   ;; curs_variables (expressed as functions).
   COLORS
   COLOR_PAIRS
   COLS
   ESCDELAY
   LINES
   TABSIZE
   curscr
   newscr
   stdscr

   ;; Colour #defines.
   COLOR_BLACK COLOR_RED COLOR_GREEN COLOR_YELLOW COLOR_BLUE COLOR_MAGENTA COLOR_CYAN COLOR_WHITE

   ;; curs_initscr
   initscr endwin

   ;; curs_window
   newwin delwin mvwin subwin derwin mvderwin dupwin wsyncup syncok wcursyncup wsyncdown

   ;; curs_refresh
   refresh wrefresh wnoutrefresh doupdate redrawwin wredrawln

   ;; curs_inopts
   cbreak nocbreak echo noecho halfdelay intrflush keypad meta nodelay raw noraw

   ;; curs_color
   start-color has-colors can-change_color init-pair init-color color-content pair-content
   init-extended-pair init-extended-color extended-color-content extended-pair-content reset-color-pairs

   ;; resizeterm
   is-term-resized resize-term resizeterm

   ;; curs_kernel
   def-prog-mode def-shell-mode reset-prog-mode reset-shell-mode resetty savetty ripoffline curs-set napms

   ;; default_colors
   use-default-colors assume-default-colors)
  (import
   (except (chezscheme) meta))

  (define library-init
    (load-shared-object "libncursesw.so.6"))

  (define-ftype window* void*)

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

  (define-syntax enum
    (syntax-rules ()
      [(_ (col val) ...)
       (begin
         (define col val) ...)]))

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
