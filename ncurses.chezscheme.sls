;; chez scheme bindings for ncurses.
;; Written by Jerry 2019-2021.
;; SPDX-License-Identifier: Unlicense
(library (ncurses)
  (export
   ERR OK

   KEY_CODE_YES KEY_MIN KEY_BREAK KEY_SRESET KEY_RESET
   KEY_DOWN KEY_UP KEY_LEFT KEY_RIGHT KEY_HOME KEY_BACKSPACE KEY_F0 KEY_DL KEY_IL KEY_DC KEY_IC KEY_EIC KEY_CLEAR KEY_EOS KEY_EOL KEY_SF KEY_SR KEY_NPAGE KEY_PPAGE KEY_STAB KEY_CTAB KEY_CATAB KEY_ENTER KEY_PRINT KEY_LL KEY_A1 KEY_A3 KEY_B2 KEY_C1 KEY_C3 KEY_BTAB KEY_BEG KEY_CANCEL KEY_CLOSE KEY_COMMAND KEY_COPY KEY_CREATE KEY_END KEY_EXIT KEY_FIND KEY_HELP KEY_MARK KEY_MESSAGE KEY_MOVE KEY_NEXT KEY_OPEN KEY_OPTIONS KEY_PREVIOUS KEY_REDO KEY_REFERENCE KEY_REFRESH KEY_REPLACE KEY_RESTART KEY_RESUME KEY_SAVE KEY_SBEG KEY_SCANCEL KEY_SCOMMAND KEY_SCOPY KEY_SCREATE KEY_SDC KEY_SDL KEY_SELECT KEY_SEND KEY_SEOL KEY_SEXIT KEY_SFIND KEY_SHELP KEY_SHOME KEY_SIC KEY_SLEFT KEY_SMESSAGE KEY_SMOVE KEY_SNEXT KEY_SOPTIONS KEY_SPREVIOUS KEY_SPRINT KEY_SREDO KEY_SREPLACE KEY_SRIGHT KEY_SRSUME KEY_SSAVE KEY_SSUSPEND KEY_SUNDO KEY_SUSPEND KEY_UNDO KEY_MOUSE KEY_RESIZE KEY_EVENT
   KEY_MAX
   KEY_F

   ;; curs_variables(3X).
   COLORS COLOR_PAIRS COLS ESCDELAY LINES TABSIZE curscr newscr stdscr

   ;; Colour #defines.
   COLOR_BLACK COLOR_RED COLOR_GREEN COLOR_YELLOW COLOR_BLUE COLOR_MAGENTA COLOR_CYAN COLOR_WHITE

   ;; curs_initscr(3X)
   initscr endwin

   ;; curs_window(3X)
   newwin delwin mvwin subwin derwin mvderwin dupwin wsyncup syncok wcursyncup wsyncdown

   ;; curs_refresh(3X)
   refresh wrefresh wnoutrefresh doupdate redrawwin wredrawln

   ;; curs_clear(3X)
   erase werase clear wclear clrtobot wclrtobot clrtoeol wclrtoeol

   ;; curs_move(3X)
   move wmove

   ;; curs_addch(3X)
   addch waddch mvaddch mvwaddch echochar wechochar
   ACS_ULCORNER ACS_LLCORNER ACS_URCORNER ACS_LRCORNER
   ACS_LTEE ACS_RTEE ACS_BTEE ACS_TTEE ACS_HLINE ACS_VLINE ACS_PLUS
   ACS_S1 ACS_S9 ACS_DIAMOND ACS_CKBOARD ACS_DEGREE ACS_PLMINUS ACS_BULLET
   ACS_LARROW ACS_RARROW ACS_DARROW ACS_UARROW ACS_BOARD ACS_LANTERN
   ACS_BLOCK ACS_S3 ACS_S7 ACS_LEQUAL ACS_GEQUAL ACS_PI ACS_NEQUAL ACS_STERLING

   ;; curs_addstr(3X)
   addstr addnstr waddstr waddnstr mvaddstr mvaddnstr mvwaddstr mvwaddnstr

   ;; curs_border(3X)
   border wborder box hline whline vline wvline mvhline mvwhline mvvline mvwvline

   ;; curs_getyx(3X), ncurses implements via macros but we can recreate via curs_legacy(3X).
   getyx getparyx getbegyx getmaxyx

   ;; curs_getch(3X) and curs_get_wch(3X)
   ;; Always export the wide versions as R6RS requires unicode.
   ;; This may be a bad idea. Revisit at some point..
   (rename
     (get-wch getch)
     (wget-wch wgetch)
     (mvget-wch mvgetch)
     (mvwget-wch mvwgetch)
     (unget-wch ungetch))
   has_key
   ;;get-wch wget-wch mvget-wch mvwget-wch unget-wch

   ;; curs_inopts(3X)
   cbreak nocbreak echo noecho halfdelay intrflush keypad meta nodelay raw noraw

   ;; curs_color(3X)
   start-color has-colors can-change-color init-pair init-color color-content pair-content
   init-extended-pair init-extended-color extended-color-content extended-pair-content reset-color-pairs

   ;; curs_attr(3X)
   attr-get wattr-get attr-set wattr-set attr-off wattr-off attr-on wattr-on attroff wattroff attron wattron attrset wattrset chgat wchgat mvchgat mvwchgat color-set wcolor-set standend wstandend standout wstandout
   A_NORMAL A_STANDOUT A_UNDERLINE A_REVERSE A_BLINK A_DIM A_BOLD A_ALTCHARSET
   A_INVIS A_PROTECT A_HORIZONTAL A_LEFT A_LOW A_RIGHT A_TOP A_VERTICAL A_ITALIC

   ;; curs_bkgd(3X)
   bkgdset wbkgdset bkgd wbkgd getbkgd

   ;; curs_beep(3X)
   beep flash

   ;; curs_termattrs(3X)
   baudrate erasechar erasewchar has_ic has_il killchar killwchar longname term_attrs termattrs termname

   ;; resizeterm(3X)
   is-term-resized resize-term resizeterm

   ;; curs_kernel(3X)
   def-prog-mode def-shell-mode reset-prog-mode reset-shell-mode resetty savetty ripoffline curs-set napms

   ;; default_colors(3X)
   use-default-colors assume-default-colors

   ;; curs_terminfo(3X)
   tigetnum

   ;; curs_legacy(3X), these require the NCURSES_OPAQUE definition.
   getattrs getbegx getbegy getcurx getcury getmaxx getmaxy getparx getpary

   ;; HACK: setlocale(3) here is linux specific.
   setlocale
   LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_ALL
   LC_PAPER LC_NAME LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT LC_IDENTIFICATION
   )
  (import
   (rename (except (chezscheme) box) (meta %meta)))

  (define-syntax auto-ptr
    (syntax-rules ()
      [(_ ((var type) ...) body body* ...)
       (let ([var (make-ftype-pointer type (foreign-alloc (ftype-sizeof type)))] ...)
         (dynamic-wind
           (lambda () #f)
           (lambda () body body* ...)
           (lambda ()
             (foreign-free (ftype-pointer-address var)) ...)))]))

  (%meta define string-map
    (lambda (func str)
      (list->string (map func (string->list str)))))

  (%meta define symbol->curses-name
    (lambda (sym)
      (string-map (lambda (c)
                    (if (eqv? c #\-)
                        #\_ c))
                  (symbol->string sym))))

  (define-syntax c_funcs
    (lambda (stx)
      (define has-chtype?
        (lambda (args)
          (find
            (lambda (x)
              (eq? 'chtype x))
            (syntax->datum args))))
      (syntax-case stx ()
        [(k (name (types ...) return))
         (has-chtype? #'(types ...))
         ;; For ncurses functions with a chtype argument, generate a calling function
         ;; that allows for those args to take either an int (as ncurses expects), or
         ;; a scheme char object.
         ;; The interface still needs to accept INTs because not all ncurses ints can
         ;; be converted to a unicode char. eg, the ACS_* chars.
         (with-syntax ([func-string (symbol->curses-name (syntax->datum #'name))]
                       [((arg arg->int) ...)
                        (map
                          (lambda (t a)
                            (list
                              a
                              (if (eq? 'chtype t)
                                  ;; 'a' is already a syntax object, so it needs to be
                                  ;; inserted into the return code syntax object as is.
                                  #`(if (char? #,a)
                                        (char->integer #,a)
                                        #,a)
                                  a)))
                          (syntax->datum #'(types ...))
                          (generate-temporaries #'(types ...)))])
           #'(define name
               (let ([f (foreign-procedure func-string (types ...) return)])
                 (lambda (arg ...)
                   (f arg->int ...)))))]
        [(_ (name args return))
         #`(define name
             (foreign-procedure #,(symbol->curses-name (syntax->datum #'name)) args return))]
        [(_ f ...)
         #`(begin
             (c_funcs f) ...)])))

  (define-syntax c/vars
    (lambda (stx)
      (syntax-case stx ()
        [(_ var type)
         #`(define-syntax var
             (identifier-syntax
               (foreign-ref 'type (foreign-entry #,(symbol->string (syntax->datum #'var))) 0)))]
        [(_ (n t) ...)
         #'(begin
             (c/vars n t) ...)])))

  (define-syntax enum
    (syntax-rules ()
      [(_ (col val) ...)
       (begin
         (define col val) ...)]))

  (define library-init
    (load-shared-object "libncursesw.so.6"))

  (define-ftype window* void*)
  (define-ftype chtype unsigned)
  (define-ftype attr_t chtype)
  ;; Redefine the Chez wchar_t type as it requires a real char, whereas our KEY_ defs
  ;; are INTs which causes problems as currently written. eg, (ungetch KEY_RESIZE).
  (define-ftype wchar_t unsigned)
  (define-ftype wint_t unsigned)

  (define-syntax acs/vars
    (lambda (stx)
      (define (offset ch)
        (* (foreign-sizeof 'unsigned) (char->integer ch)))
      (syntax-case stx ()
        [(_ var offset-char)
         #`(define-syntax var
             (identifier-syntax
               (foreign-ref 'unsigned (foreign-entry "acs_map") #,(offset (syntax->datum #'offset-char)))))]
        [(_ (n o) ...)
         #'(begin
             (acs/vars n o) ...)])))

  (acs/vars
    (ACS_ULCORNER	#\l)
    (ACS_LLCORNER	#\m)
    (ACS_URCORNER	#\k)
    (ACS_LRCORNER	#\j)
    (ACS_LTEE		#\t)
    (ACS_RTEE		#\u)
    (ACS_BTEE		#\v)
    (ACS_TTEE		#\w)
    (ACS_HLINE		#\q)
    (ACS_VLINE		#\x)
    (ACS_PLUS		#\n)
    (ACS_S1		#\o)
    (ACS_S9		#\s)
    (ACS_DIAMOND	#\`)
    (ACS_CKBOARD	#\a)
    (ACS_DEGREE		#\f)
    (ACS_PLMINUS	#\g)
    (ACS_BULLET		#\~)
    (ACS_LARROW		#\,)
    (ACS_RARROW		#\+)
    (ACS_DARROW		#\.)
    (ACS_UARROW		#\-)
    (ACS_BOARD		#\h)
    (ACS_LANTERN	#\i)
    (ACS_BLOCK		#\0)
    (ACS_S3		#\p)
    (ACS_S7		#\r)
    (ACS_LEQUAL		#\y)
    (ACS_GEQUAL		#\z)
    (ACS_PI		#\{)
    (ACS_NEQUAL		#\|)
    (ACS_STERLING	#\})
    )

  (enum
   (ERR	-1)
   (OK	0))

  (enum
   (KEY_CODE_YES    #o400)
   (KEY_MIN         #o401)
   (KEY_BREAK       #o401)
   (KEY_SRESET      #o530)
   (KEY_RESET       #o531)

   (KEY_DOWN        #o402)
   (KEY_UP          #o403)
   (KEY_LEFT        #o404)
   (KEY_RIGHT       #o405)
   (KEY_HOME        #o406)
   (KEY_BACKSPACE   #o407)
   (KEY_F0          #o410)
   (KEY_DL          #o510)
   (KEY_IL          #o511)
   (KEY_DC          #o512)
   (KEY_IC          #o513)
   (KEY_EIC         #o514)
   (KEY_CLEAR       #o515)
   (KEY_EOS         #o516)
   (KEY_EOL         #o517)
   (KEY_SF          #o520)
   (KEY_SR          #o521)
   (KEY_NPAGE       #o522)
   (KEY_PPAGE       #o523)
   (KEY_STAB        #o524)
   (KEY_CTAB        #o525)
   (KEY_CATAB       #o526)
   (KEY_ENTER       #o527)
   (KEY_PRINT       #o532)
   (KEY_LL          #o533)
   (KEY_A1          #o534)
   (KEY_A3          #o535)
   (KEY_B2          #o536)
   (KEY_C1          #o537)
   (KEY_C3          #o540)
   (KEY_BTAB        #o541)
   (KEY_BEG         #o542)
   (KEY_CANCEL      #o543)
   (KEY_CLOSE       #o544)
   (KEY_COMMAND     #o545)
   (KEY_COPY        #o546)
   (KEY_CREATE      #o547)
   (KEY_END         #o550)
   (KEY_EXIT        #o551)
   (KEY_FIND        #o552)
   (KEY_HELP        #o553)
   (KEY_MARK        #o554)
   (KEY_MESSAGE     #o555)
   (KEY_MOVE        #o556)
   (KEY_NEXT        #o557)
   (KEY_OPEN        #o560)
   (KEY_OPTIONS     #o561)
   (KEY_PREVIOUS    #o562)
   (KEY_REDO        #o563)
   (KEY_REFERENCE   #o564)
   (KEY_REFRESH     #o565)
   (KEY_REPLACE     #o566)
   (KEY_RESTART     #o567)
   (KEY_RESUME      #o570)
   (KEY_SAVE        #o571)
   (KEY_SBEG        #o572)
   (KEY_SCANCEL     #o573)
   (KEY_SCOMMAND    #o574)
   (KEY_SCOPY       #o575)
   (KEY_SCREATE     #o576)
   (KEY_SDC         #o577)
   (KEY_SDL         #o600)
   (KEY_SELECT      #o601)
   (KEY_SEND        #o602)
   (KEY_SEOL        #o603)
   (KEY_SEXIT       #o604)
   (KEY_SFIND       #o605)
   (KEY_SHELP       #o606)
   (KEY_SHOME       #o607)
   (KEY_SIC         #o610)
   (KEY_SLEFT       #o611)
   (KEY_SMESSAGE    #o612)
   (KEY_SMOVE       #o613)
   (KEY_SNEXT       #o614)
   (KEY_SOPTIONS    #o615)
   (KEY_SPREVIOUS   #o616)
   (KEY_SPRINT      #o617)
   (KEY_SREDO       #o620)
   (KEY_SREPLACE    #o621)
   (KEY_SRIGHT      #o622)
   (KEY_SRSUME      #o623)
   (KEY_SSAVE       #o624)
   (KEY_SSUSPEND    #o625)
   (KEY_SUNDO       #o626)
   (KEY_SUSPEND     #o627)
   (KEY_UNDO        #o630)
   (KEY_MOUSE       #o631)
   (KEY_RESIZE      #o632)
   (KEY_EVENT       #o633)

   (KEY_MAX         #o777))

  (define KEY_F
    (lambda (n)
      (+ KEY_F0 n)))

  (c/vars
   (COLORS	int)
   (COLOR_PAIRS	int)
   (COLS	int)
   (ESCDELAY	int)
   (LINES	int)
   (TABSIZE	int)
   (curscr	void*)
   (newscr	void*)
   (stdscr	void*))

  (enum
   (COLOR_BLACK		0)
   (COLOR_RED		1)
   (COLOR_GREEN		2)
   (COLOR_YELLOW	3)
   (COLOR_BLUE		4)
   (COLOR_MAGENTA	5)
   (COLOR_CYAN		6)
   (COLOR_WHITE		7))

  ;; curs_attr IDs.
  ;; attr-bits is written using syntax-case/with-syntax so that the bitmask is calculated at compile time.
  (define-syntax attr-bits
    (lambda (x)
      (syntax-case x ()
        [(_ bitnum)
         (with-syntax ([b (bitwise-arithmetic-shift-left 1 (+ 8 (syntax->datum #'bitnum)))])
           #'b)])))

  ;; TODO add the rest. Only the basic ones are here ATM.
  (enum
    (A_NORMAL		0)
    (A_STANDOUT		(attr-bits 8))
    (A_UNDERLINE	(attr-bits 9))
    (A_REVERSE		(attr-bits 10))
    (A_BLINK		(attr-bits 11))
    (A_DIM		(attr-bits 12))
    (A_BOLD		(attr-bits 13))
    (A_ALTCHARSET	(attr-bits 14))
    (A_INVIS		(attr-bits 15))
    (A_PROTECT		(attr-bits 16))
    (A_HORIZONTAL	(attr-bits 17))
    (A_LEFT		(attr-bits 18))
    (A_LOW		(attr-bits 19))
    (A_RIGHT		(attr-bits 20))
    (A_TOP		(attr-bits 21))
    (A_VERTICAL		(attr-bits 22))
    (A_ITALIC		(attr-bits 23)))

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

   ;; curs_move(3X)
   (move (int int) int)
   (wmove (window* int int) int)

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

   ;; curs_getch
   (getch () int)
   (wgetch (window*) int)
   (mvgetch (int int) int)
   (mvwgetch (window* int int) int)
   (ungetch (int) int)
   (has_key (int) boolean)

   ;; curs_get_wch(3X)
   (get_wch ((* wint_t)) int)
   (wget_wch (window* (* wint_t)) int)
   (mvget_wch (int int (* wint_t)) int)
   (mvwget_wch (window* int int (* wint_t)) int)
   (unget-wch (wchar_t) int)

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
   (can-change-color () boolean)
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
   (attr_off (attr_t void*) int)
   (wattr_off (window* attr_t void*) int)
   (attr_on (attr_t void*) int)
   (wattr_on (window* attr_t void*) int)
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

   ;; curs_termattrs
   (baudrate () int)
   (erasechar () char)
   (erasewchar ((* wchar_t)) int)
   (has_ic () boolean)
   (has_il () boolean)
   (killchar () char)
   (killwchar ((* wchar_t)) int)
   (longname () string)
   (term_attrs () attr_t)
   (termattrs () chtype)
   (termname () string)

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
   (assume-default-colors (int int) int)

   ;; curs_terminfo
   (tigetnum (string) int)

   ;; curs_legacy
   (getattrs (window*) int)
   (getbegx (window*) int)
   (getbegy (window*) int)
   (getcurx (window*) int)
   (getcury (window*) int)
   (getmaxx (window*) int)
   (getmaxy (window*) int)
   (getparx (window*) int)
   (getpary (window*) int))

  (define-syntax define-wch-func
    (lambda (x)
      (syntax-case x ()
        [(k name args ...)
         (with-syntax ([curs-name (datum->syntax #'k
                                    (string->symbol
                                      (symbol->curses-name (syntax->datum #'name))))])
           #'(define name
               (lambda (args ...)
                 (auto-ptr ([mem wint_t])
                   (let ([rc (curs-name args ... mem)])
                     (cond
                       ;; rc will be OK on regular char or KEY_CODE_YES for a function key.
                       [(fx=? rc ERR)
                        (error 'name "error" rc)]
                       [else
                         (ftype-ref wint_t () mem)]))))))])))
  (define-wch-func get-wch)
  (define-wch-func wget-wch win)
  (define-wch-func mvget-wch y x)
  (define-wch-func mvwget-wch win y x)

  ;; curs_getyx, ncurses implements via macros but we can recreate via curs_legacy.
  (define getyx
    (lambda (w)
      (values (getcury w) getcurx w)))

  (define getparyx
    (lambda (w)
      (values (getpary w) (getparx w))))

  (define getbegyx
    (lambda (w)
      (values (getbegy w) (getbegx w))))

  (define getmaxyx
    (lambda (w)
      (values (getmaxy w) (getmaxx w))))

  ;;;;;
  ;; None of the pure attribute functions use the opt argument.
  ;; See curs_attr(3X) for more detail.
  (define attr-off
    (lambda (attr)
      (attr_off attr 0)))

  (define wattr-off
    (lambda (win attr)
      (wattr_off win attr 0)))

  (define attr-on
    (lambda (attr)
      (attr_on attr 0)))

  (define wattr-on
    (lambda (win attr)
      (wattr_on win attr 0)))

   ;; HACK: setlocale here is linux specific.
  (c_funcs
    ;; char *setlocale(int category, const char *locale);
    (setlocale (int string) string))

  (enum
    (LC_CTYPE		 0)
    (LC_NUMERIC		 1)
    (LC_TIME		 2)
    (LC_COLLATE		 3)
    (LC_MONETARY	 4)
    (LC_MESSAGES	 5)
    (LC_ALL		 6)
    (LC_PAPER		 7)
    (LC_NAME		 8)
    (LC_ADDRESS		 9)
    (LC_TELEPHONE	10)
    (LC_MEASUREMENT	11)
    (LC_IDENTIFICATION	12))
  )
