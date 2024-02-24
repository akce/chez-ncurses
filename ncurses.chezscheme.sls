;; chez scheme bindings for ncurses.
;; Written by Jerry 2019-2024.
;; SPDX-License-Identifier: Unlicense
(library (ncurses)
  (export
   ERR OK

   ncurses-error?

   key-ncint key-symchar
   KEY_ESCAPE
   KEY_CODE_YES KEY_MIN KEY_BREAK KEY_SRESET KEY_RESET
   KEY_DOWN KEY_UP KEY_LEFT KEY_RIGHT KEY_HOME KEY_BACKSPACE KEY_F0 KEY_DL KEY_IL KEY_DC KEY_IC KEY_EIC KEY_CLEAR KEY_EOS KEY_EOL KEY_SF KEY_SR KEY_NPAGE KEY_PPAGE KEY_STAB KEY_CTAB KEY_CATAB KEY_ENTER KEY_PRINT KEY_LL KEY_A1 KEY_A3 KEY_B2 KEY_C1 KEY_C3 KEY_BTAB KEY_BEG KEY_CANCEL KEY_CLOSE KEY_COMMAND KEY_COPY KEY_CREATE KEY_END KEY_EXIT KEY_FIND KEY_HELP KEY_MARK KEY_MESSAGE KEY_MOVE KEY_NEXT KEY_OPEN KEY_OPTIONS KEY_PREVIOUS KEY_REDO KEY_REFERENCE KEY_REFRESH KEY_REPLACE KEY_RESTART KEY_RESUME KEY_SAVE KEY_SBEG KEY_SCANCEL KEY_SCOMMAND KEY_SCOPY KEY_SCREATE KEY_SDC KEY_SDL KEY_SELECT KEY_SEND KEY_SEOL KEY_SEXIT KEY_SFIND KEY_SHELP KEY_SHOME KEY_SIC KEY_SLEFT KEY_SMESSAGE KEY_SMOVE KEY_SNEXT KEY_SOPTIONS KEY_SPREVIOUS KEY_SPRINT KEY_SREDO KEY_SREPLACE KEY_SRIGHT KEY_SRSUME KEY_SSAVE KEY_SSUSPEND KEY_SUNDO KEY_SUSPEND KEY_UNDO KEY_MOUSE KEY_RESIZE
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
   #;attr-get wattr-get #;attr-set wattr-set #;attr-off #;wattr-off #;attr-on #;wattr-on
   ;; Remove legacy setters, use underscored versions instead.
   ;;attroff wattroff attron wattron attrset wattrset

   chgat wchgat mvchgat mvwchgat color-set wcolor-set standend wstandend standout wstandout
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
   tigetflag tigetnum tigetstr

   ;; curs_mouse(3X)
   mousemask has-mouse ungetmouse mouseinterval wenclose wmouse-trafo mouse-trafo
   ;; call-with-mouse-event replaces getmouse in this API.
   call-with-mouse-event
   ;; mevent provides access to the MEVENT struct.
   mevent-id mevent-x mevent-y mevent-z mevent-bstate
   BUTTON1_RELEASED BUTTON1_PRESSED BUTTON1_CLICKED BUTTON1_DOUBLE_CLICKED BUTTON1_TRIPLE_CLICKED
   BUTTON2_RELEASED BUTTON2_PRESSED BUTTON2_CLICKED BUTTON2_DOUBLE_CLICKED BUTTON2_TRIPLE_CLICKED
   BUTTON3_RELEASED BUTTON3_PRESSED BUTTON3_CLICKED BUTTON3_DOUBLE_CLICKED BUTTON3_TRIPLE_CLICKED
   BUTTON4_RELEASED BUTTON4_PRESSED BUTTON4_CLICKED BUTTON4_DOUBLE_CLICKED BUTTON4_TRIPLE_CLICKED
   BUTTON5_RELEASED BUTTON5_PRESSED BUTTON5_CLICKED BUTTON5_DOUBLE_CLICKED BUTTON5_TRIPLE_CLICKED
   BUTTON_CTRL BUTTON_SHIFT BUTTON_ALT REPORT_MOUSE_POSITION

   ;; curs_legacy(3X), these require the NCURSES_OPAQUE definition.
   getattrs getbegx getbegy getcurx getcury getmaxx getmaxy getparx getpary

   ;; HACK: setlocale(3) here is linux specific.
   setlocale
   LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_ALL
   LC_PAPER LC_NAME LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT LC_IDENTIFICATION
   )
  (import
    (rename (chezscheme) (box %box) (meta %meta))
    (ncurses common))

  ;; Load ncurses from a few places.
  ;; [return] #f (for ncurses linked to chez-scheme) or name of loaded library, error exception on failure.
  ;;
  ;; Default load order is:
  ;; - ncurses linked to Chez Scheme binary (in any),
  ;; - libncursesw.so.6 (ncurses built with --enable-widec)
  ;; - libncurses.so.6 (this could include wide chars if --enable-widec --disable-lib-suffixes)
  ;;
  ;; ncurses is a little special in that it's an optional dependency of Chez Scheme itself and
  ;; used by it for the expression editor. Therefore, it's highly unlikely not to be linked in.
  ;; So for those special cases where it's not linked, fall back to the standard libnames.
  ;;
  ;; TODO provide a method to override based on makefile or config file?
  (define library-init
    (let ([lib-order
            '(
              #f			; load ncurses linked to scheme binary (if any).
              "libncursesw.so.6"
              "libncurses.so.6"
              )])
      (let loop ([libs lib-order])
        (cond
          [(null? libs)
           (errorf 'ncurses-library-load "Failed to load from: ~s" lib-order)]
          [(and
             (guard
               (e [else #f])
               (load-shared-object (car libs))
               #t)
             ;; This foreign-entry? check is only useful for the #f (internal) case,
             ;; but it's harmless to do for each lib.
             (foreign-entry? "initscr"))
             (car libs)]
          [else
            (loop (cdr libs))]))))

  ;; Track the last version of ncurses.h that these bindings match.
  ;; It's not public or used in any way, it's only informational; update with changes.
  (define NCURSES_VERSION_MAJOR 6)
  (define NCURSES_VERSION_MINOR 4)
  (define NCURSES_VERSION_PATCH 20221231)
  (define NCURSES_MOUSE_VERSION 2)

  ;; Create the ftype pointer with each call as ncurses does not initialise the address
  ;; until after initscr is called.
  (define-syntax c/vars
    (lambda (stx)
      (syntax-case stx ()
        [(_ var type)
         #`(define-syntax var
             (identifier-syntax
               (ftype-ref type ()
                          (make-ftype-pointer type (foreign-entry #,(symbol->string (syntax->datum #'var)))))))]
        [(_ (n t) ...)
         #'(begin
             (c/vars n t) ...)])))

  ;; [proc] return ftypes (* unsigned-8) as a UTF8 string.
  (define u8*->string
    (lambda (fptr)
      (utf8->string
       (let f ([i 0])
         (let ([c (ftype-ref unsigned-8 () fptr i)])
           (if (fx= c 0)
             (make-bytevector i)
             (let ([bv (f (fx+ i 1))])
               (bytevector-u8-set! bv i c)
               bv)))))))

  (define-ftype attr_t chtype)
  ;; Redefine Chez Scheme's wchar_t type as it requires a real char, whereas our KEY_ defs
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

  (define-syntax define-keys
    (lambda (x)
      (syntax-case x ()
        [(_ (ncint-getter keysymchar-getter)
            (name value) ...)
         #'(begin
             (define-values
               (ncint-table keysymchar-table)
               (let ([key-ht (make-eq-hashtable)]
                     [sym-ht (make-eqv-hashtable)]
                     [KEY_F0 #o410])
                 (hashtable-set! key-ht 'name value) ...
                 (hashtable-set! sym-ht value 'name) ...
                 ;; Expand and add function keys. There's room for 64.
                 (do
                   ([i 1 (fx+ i 1)])
                   [(fx>? i 64) (values key-ht sym-ht)]
                   (let ([k (string->symbol (string-append "KEY_F" (number->string i)))]
                         [v (fx+ KEY_F0 i)])
                     (hashtable-set! key-ht k v)
                     (hashtable-set! sym-ht v k)))))
             (define ncint-getter
               (lambda (charsym)
                 (cond
                   [(hashtable-ref ncint-table charsym #f)
                    => values]
                   [(char? charsym)
                    (char->integer charsym)]
                   [else
                     (error 'ncint-getter "Unknown ncurses KEY_ symbol or invalid input" charsym)])))
             (define keysymchar-getter
               (lambda (key-num)
                 ;; hashtable-ref is a procedure so doing this way saves an integer->char
                 ;; conversion if we were to put integer->char in the hashtable-ref call
                 ;; for cases where key-num is in the hashtable.
                 (cond
                   [(hashtable-ref keysymchar-table key-num #f)
                    => values]
                   [else
                     ;; Should we cache the converted char?
                     (integer->char key-num)])))
             ;; Using tables and symbols is a serious break in the API, alert
             ;; anyone who's still using the previous method to update their code.
             ;; TODO remove this after a half year or so, eg. end of 2024.
             (define-syntax name
               (identifier-syntax
                 (errorf 'name "chez-ncurses key handing has changed. Use (~s '~s) instead." 'ncint-getter 'name))) ...)])))

  ;; key-ncint KEY_SYMBOL | Scheme Char -> NCurses KEY INT
  ;; key-symchar NCurses KEY INT -> KEY_SYMBOL | Scheme Char
  ;;
  ;; Creates two functions, one to convert ncurses key integer values (as returned by
  ;; getch and get-wch) to KEY_SYMBOL or Scheme Character types. The other performs this
  ;; operation in reverse and is handy for ungetch.
  (define-keys (key-ncint key-symchar)
    ;; KEY_ESCAPE is a chez-ncurses addition.
    ;; It's here because it's useful to detect ALT key combos.
    ;; TODO Add the rest of the ASCII control chars?
    ;; NOTE ASCII form-feed (^L) is #\page in Chez Scheme.
    ;; TODO rename KEY_ESCAPE to ASCII_ESCAPE? Or drop prefix altogether?
    (KEY_ESCAPE      #o033)

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
    )

  ;; Internal ncurses.h definition not in getch(3).
  (define KEY_MAX #o777)

  (define KEY_F
    (lambda (n)
      (fx+ KEY_F0 n)))

  (c/vars
   (COLORS	int)
   (COLOR_PAIRS	int)
   (COLS	int)
   (ESCDELAY	int)
   (LINES	int)
   (TABSIZE	int)
   (curscr	window*)
   (newscr	window*)
   (stdscr	window*))

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
   ;; curs_initscr(3X)
   (initscr () (* window))
   (endwin () errok)

   ;; curs_window(3X)
   (newwin (int int int int) (* window))
   (delwin ((* window)) errok)
   (mvwin ((* window) int int) errok)
   (subwin ((* window) int int int int) (* window))
   (derwin ((* window) int int int int) (* window))
   (mvderwin ((* window) int int) errok)
   (dupwin ((* window)) (* window))
   (wsyncup ((* window)) void)
   (syncok ((* window) bool) errok)
   (wcursyncup ((* window)) void)
   (wsyncdown ((* window)) void)

   ;; curs_refresh(3X)
   (refresh () errok)
   (wrefresh ((* window)) errok)
   (wnoutrefresh ((* window)) errok)
   (doupdate () errok)
   (redrawwin ((* window)) errok)
   (wredrawln ((* window) int int) errok)

   ;; curs_clear(3X)
   (erase () errok)
   (werase ((* window)) errok)
   (clear () errok)
   (wclear ((* window)) errok)
   (clrtobot () errok)
   (wclrtobot ((* window)) errok)
   (clrtoeol () errok)
   (wclrtoeol ((* window)) errok)

   ;; curs_move(3X)
   (move (int int) errok)
   (wmove ((* window) int int) errok)

   ;; curs_addch(3X)
   (addch (chtype) errok)
   (waddch ((* window) chtype) errok)
   (mvaddch (int int chtype) errok)
   (mvwaddch ((* window) int int chtype) errok)
   (echochar (chtype) errok)
   (wechochar ((* window) chtype) errok)

   ;; curs_addstr(3X)
   (addstr (string) errok)
   (addnstr (string int) errok)
   (waddstr ((* window) string) errok)
   (waddnstr ((* window) string int) errok)
   (mvaddstr (int int string) errok)
   (mvaddnstr (int int string int) errok)
   (mvwaddstr ((* window) int int string) errok)
   (mvwaddnstr ((* window) int int string int) errok)

   ;; curs_border(3X)
   (border (chtype chtype chtype chtype chtype chtype chtype chtype) errok)
   (wborder ((* window) chtype chtype chtype chtype chtype chtype chtype chtype) errok)
   (box ((* window) chtype chtype) errok)
   (hline (chtype int) errok)
   (whline ((* window) chtype int) errok)
   (vline (chtype int) errok)
   (wvline ((* window) chtype int) errok)
   (mvhline (int int chtype int) errok)
   (mvwhline ((* window) int int chtype int) errok)
   (mvvline (int int chtype int) errok)
   (mvwvline ((* window) int int chtype int) errok)

   ;; curs_getch(3X)
   ;; Of these, only ungetch returns OK on success.
   (getch () errok)
   (wgetch ((* window)) errok)
   (mvgetch (int int) errok)
   (mvwgetch ((* window) int int) errok)
   (ungetch (int) errok)
   (has_key (int) bool)

   ;; curs_get_wch(3X)
   ;; TODO combine syntax transformers for these?
   (get_wch ((* wint_t)) int)
   (wget_wch ((* window) (* wint_t)) int)
   (mvget_wch (int int (* wint_t)) int)
   (mvwget_wch ((* window) int int (* wint_t)) int)
   (unget-wch (wchar_t) errok)

   ;; curs_inopts(3X)
   (cbreak() errok)
   (nocbreak() errok)
   (echo () errok)
   (noecho () errok)
   (halfdelay (int) errok)
   (intrflush ((* window) bool) errok)
   (keypad ((* window) bool) errok)
   (meta ((* window) bool) errok)
   (nodelay ((* window) bool) errok)
   (raw () errok)
   (noraw () errok)

   ;; curs_color(3X)
   (start-color () errok)
   (has-colors () bool)
   (can-change-color () bool)
   (init-pair (short short short) errok)
   (init-color (short short short short) errok)
   (color-content (short (* short) (* short) (* short)) errok)
   (pair-content (short (* short) (* short)) errok)
   ;; extensions
   (init-extended-pair (int int int) errok)
   (init-extended-color (int int int int) errok)
   (extended-color-content (short (* short) (* short) (* short)) errok)
   (extended-pair-content (short (* short) (* short)) errok)
   (reset-color-pairs () void)

   ;; curs_attr(3X)
   #;(attr-get ((* attr_t) (* short) void*) errok)
   ;; wattr_get is complex in that memory address args are used to retrieve values.
   ;; The wattr-get wrapper will handle errok processing.
   (wattr_get ((* window) (* attr_t) (* short) (* int)) int)
   #;(attr-set (attr_t short void*) errok)
   (wattr_set ((* window) attr_t short (* int)) errok)
   #;(attr_off (attr_t void*) errok)
   #;(wattr_off ((* window) attr_t void*) errok)
   #;(attr_on (attr_t void*) errok)
   #;(wattr_on ((* window) attr_t void*) errok)
   #;(attroff (int) errok)
   #;(wattroff ((* window) int) errok)
   #;(attron (int) errok)
   #;(wattron ((* window) int) errok)
   #;(attrset (int) errok)
   #;(wattrset ((* window) int) errok)

   (chgat (int attr_t short void*) errok)
   (wchgat ((* window) int attr_t short void*) errok)
   (mvchgat (int int int attr_t short void*) errok)
   (mvwchgat ((* window) int int int attr_t short void*) errok)
   (color-set (short void*) errok)
   (wcolor-set ((* window) short void*) errok)
   (standend () errok)
   (wstandend ((* window)) errok)
   (standout () errok)
   (wstandout ((* window)) errok)

   ;; curs_bkgd(3X)
   (bkgdset (chtype) void)
   (wbkgdset ((* window) chtype) void)
   (bkgd (chtype) errok)
   (wbkgd ((* window) chtype) errok)
   ;; TODO implement this return conversion.
   (getbkgd ((* window)) chtype)

   ;; curs_beep(3X)
   (beep () errok)
   (flash () errok)

   ;; curs_termattrs(3X)
   ;; TODO check longname and termname as they return NULL on error.
   (baudrate () errok)
   (erasechar () char)
   (erasewchar ((* wchar_t)) errok)
   (has_ic () bool)
   (has_il () bool)
   (killchar () char)
   (killwchar ((* wchar_t)) errok)
   (longname () string)
   (term_attrs () attr_t)
   (termattrs () chtype)
   (termname () string)

   ;; resizeterm(3X)
   (is-term-resized (int int) bool)
   (resize-term (int int) errok)
   (resizeterm (int int) errok)

   ;; curs_kernel(3X)
   ;; int return by design: only curs-set potentially returns ERR.
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
   (curs-set (int) errok)
   (napms (int) int)

   ;; default_colors(3X)
   (use-default-colors () errok)
   (assume-default-colors (int int) errok)

   ;; curs_legacy(3X)
   (getattrs ((* window)) errok)
   (getbegx ((* window)) errok)
   (getbegy ((* window)) errok)
   (getcurx ((* window)) errok)
   (getcury ((* window)) errok)
   (getmaxx ((* window)) errok)
   (getmaxy ((* window)) errok)
   (getparx ((* window)) errok)
   (getpary ((* window)) errok)
   )

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
                        (ncurses-error 'name)]
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
  ;; See curs_attr(3X) for more detail.
  ;; `opt` is used for extended colours and is used in this API as `pair` is the same
  ;; colour value except truncated to short.

  (define wattr-get
    (lambda (win)
      (auto-ptr ([attr attr_t]
                 [opts int])
        (let ([rc (wattr_get win attr (make-ftype-pointer short 0) opts)])
          (cond
            [(= rc OK)
             (values (ftype-ref attr_t () attr) (ftype-ref int () opts))]
            [else
              (ncurses-error 'wattr-get)])))))

  (define wattr-set
    (case-lambda
      [(win attr)
       (wattr_set win attr 0 (make-ftype-pointer int 0))]
      [(win attr opts)
       (auto-ptr ([mem int])
         (ftype-set! int () mem opts)
         (wattr_set win attr 0 mem))]))

  #;(define attr-off
    (lambda (attr)
      (attr_off attr 0)))

  #;(define wattr-off
    (lambda (win attr)
      (wattr_off win attr 0)))

  #;(define attr-on
    (lambda (attr)
      (attr_on attr 0)))

  #;(define wattr-on
    (lambda (win attr)
      (wattr_on win attr 0)))

  ;; curs_terminfo(3X)
  (define tigetflag
    (lambda (str)
      (let ([rc ((foreign-procedure "tigetflag" (string) int) str)])
        (cond
          [(= rc 0)
           (errorf 'tigetflag "~s cancelled or absent from terminal description" str)]
          [(= rc -1)
           (errorf 'tigetflag "~s is not a boolean capability" str)]
          [else
            rc]))))

  (define tigetnum
    (lambda (str)
      (let ([rc ((foreign-procedure "tigetnum" (string) int) str)])
        (cond
          [(= rc -1)
           (errorf 'tigetnum "~s cancelled or absent from terminal description" str)]
          [(= rc -2)
           (errorf 'tigetnum "~s is not a numeric capability" str)]
          [else
            rc]))))

  (define tigetstr
    (lambda (str)
      (let ([addr ((foreign-procedure "tigetstr" (string) iptr) str)])
        (cond
          [(= addr 0)
           (errorf 'tigetstr "~s cancelled or absent from terminal description" str)]
          [(= addr -1)
           (errorf 'tigetstr "~s is not a string capability" str)]
          [else
            (u8*->string (make-ftype-pointer unsigned-8 addr))]))))

  ;;;;;; curs_mouse(3X)

  (define-syntax NCURSES_MOUSE_MASK
    (lambda (x)
      (syntax-case x ()
        [(_ button mode)
         #'(bitwise-arithmetic-shift-left mode (fx* (fx- button 1) 5))])))

  (enum
    (NCURSES_BUTTON_RELEASED	#o01)
    (NCURSES_BUTTON_PRESSED	#o02)
    (NCURSES_BUTTON_CLICKED	#o04)
    (NCURSES_DOUBLE_CLICKED	#o10)
    (NCURSES_TRIPLE_CLICKED	#o20)
    (NCURSES_RESERVED_EVENT	#o40))

  (enum
    (BUTTON1_RELEASED		(NCURSES_MOUSE_MASK 1 NCURSES_BUTTON_RELEASED))
    (BUTTON1_PRESSED		(NCURSES_MOUSE_MASK 1 NCURSES_BUTTON_PRESSED))
    (BUTTON1_CLICKED		(NCURSES_MOUSE_MASK 1 NCURSES_BUTTON_CLICKED))
    (BUTTON1_DOUBLE_CLICKED	(NCURSES_MOUSE_MASK 1 NCURSES_DOUBLE_CLICKED))
    (BUTTON1_TRIPLE_CLICKED	(NCURSES_MOUSE_MASK 1 NCURSES_TRIPLE_CLICKED))

    (BUTTON2_RELEASED		(NCURSES_MOUSE_MASK 2 NCURSES_BUTTON_RELEASED))
    (BUTTON2_PRESSED		(NCURSES_MOUSE_MASK 2 NCURSES_BUTTON_PRESSED))
    (BUTTON2_CLICKED		(NCURSES_MOUSE_MASK 2 NCURSES_BUTTON_CLICKED))
    (BUTTON2_DOUBLE_CLICKED	(NCURSES_MOUSE_MASK 2 NCURSES_DOUBLE_CLICKED))
    (BUTTON2_TRIPLE_CLICKED	(NCURSES_MOUSE_MASK 2 NCURSES_TRIPLE_CLICKED))

    (BUTTON3_RELEASED		(NCURSES_MOUSE_MASK 3 NCURSES_BUTTON_RELEASED))
    (BUTTON3_PRESSED		(NCURSES_MOUSE_MASK 3 NCURSES_BUTTON_PRESSED))
    (BUTTON3_CLICKED		(NCURSES_MOUSE_MASK 3 NCURSES_BUTTON_CLICKED))
    (BUTTON3_DOUBLE_CLICKED	(NCURSES_MOUSE_MASK 3 NCURSES_DOUBLE_CLICKED))
    (BUTTON3_TRIPLE_CLICKED	(NCURSES_MOUSE_MASK 3 NCURSES_TRIPLE_CLICKED))

    (BUTTON4_RELEASED		(NCURSES_MOUSE_MASK 4 NCURSES_BUTTON_RELEASED))
    (BUTTON4_PRESSED		(NCURSES_MOUSE_MASK 4 NCURSES_BUTTON_PRESSED))
    (BUTTON4_CLICKED		(NCURSES_MOUSE_MASK 4 NCURSES_BUTTON_CLICKED))
    (BUTTON4_DOUBLE_CLICKED	(NCURSES_MOUSE_MASK 4 NCURSES_DOUBLE_CLICKED))
    (BUTTON4_TRIPLE_CLICKED	(NCURSES_MOUSE_MASK 4 NCURSES_TRIPLE_CLICKED))

    (BUTTON5_RELEASED		(NCURSES_MOUSE_MASK 5 NCURSES_BUTTON_RELEASED))
    (BUTTON5_PRESSED		(NCURSES_MOUSE_MASK 5 NCURSES_BUTTON_PRESSED))
    (BUTTON5_CLICKED		(NCURSES_MOUSE_MASK 5 NCURSES_BUTTON_CLICKED))
    (BUTTON5_DOUBLE_CLICKED	(NCURSES_MOUSE_MASK 5 NCURSES_DOUBLE_CLICKED))
    (BUTTON5_TRIPLE_CLICKED	(NCURSES_MOUSE_MASK 5 NCURSES_TRIPLE_CLICKED))

    (BUTTON_CTRL		(NCURSES_MOUSE_MASK 6 #o001))
    (BUTTON_SHIFT		(NCURSES_MOUSE_MASK 6 #o002))
    (BUTTON_ALT			(NCURSES_MOUSE_MASK 6 #o004))
    (REPORT_MOUSE_POSITION	(NCURSES_MOUSE_MASK 6 #o010)))

  (define-ftype mmask_t unsigned-long)
  (define-ftype mevent
    (struct
      (id	short)
      (x	int)
      (y	int)
      (z	int)
      (bstate	mmask_t)))

  (define mevent?
    (lambda (m*)
      (ftype-pointer? mevent m*)))

  (define mevent-id
    (lambda (m*)
      (ftype-ref mevent (id) m*)))

  (define mevent-x
    (lambda (m*)
      (ftype-ref mevent (x) m*)))

  (define mevent-y
    (lambda (m*)
      (ftype-ref mevent (y) m*)))

  (define mevent-z
    (lambda (m*)
      (ftype-ref mevent (z) m*)))

  (define mevent-bstate
    (lambda (m*)
      (ftype-ref mevent (bstate) m*)))

  (define mousemask
    (let ([c/func (foreign-procedure "mousemask" (mmask_t (* mmask_t)) mmask_t)])
      (lambda (new-mask)
        (auto-ptr ([old-mask* mmask_t])
          (let ([actual-mask (c/func new-mask old-mask*)])
            (values actual-mask (ftype-ref mmask_t () old-mask*)))))))

  ;; call-with-mouse-event calls `procedure` with a mouse event only when there's a valid mouse event available.
  ;; ie, call only if the underlying ncurses getmouse function returns OK.
  ;; The mouse event object is only valid while procedure is executing, callers must not store references to
  ;; it outside of `procedure`. This implies that `ungetmouse` is only valid from within `procedure`.
  (define call-with-mouse-event
    (let ([c/func (foreign-procedure "getmouse" ((* mevent)) int)])
      (lambda (procedure)
        (let* ([m* (make-ftype-pointer mevent (foreign-alloc (ftype-sizeof mevent)))]
               [rc (c/func m*)])
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (if (fx=? rc OK)
                (procedure m*)
                ERR))
            (lambda ()
              (foreign-free (ftype-pointer-address m*))))))))

  (c_funcs
    (has-mouse () bool)
    (ungetmouse ((* mevent)) int)
    ;; Should i case-lambda `mouseinterval` so no args is the query (-1) version?
    (mouseinterval (int) int)
    (wenclose ((* window) int int) bool)
    )

  ;; Throws an exception rather than return an error code as this function returns `values` on success.
  (define wmouse-trafo
    (let ([c/func (foreign-procedure "wmouse_trafo" ((* window) (* int) (* int) bool) bool)])
      (lambda (win y x to-screen?)
        (auto-ptr ([x* int]
                   [y* int])
          (ftype-set! int () y* y)
          (ftype-set! int () x* x)
          (let ([rc (c/func win y* x* (if to-screen? 1 0))])
            (cond
              [(fx=? rc 1)
                (values (ftype-ref int () y*) (ftype-ref int () x*))]
              [else
                (ncurses-error
                  'wmouse-trafo "Error in params or translated points not within window" win y x to-screen?)]))))))

  (define mouse-trafo
    (lambda (y x to-screen?)
      ;; curs_mouse(3X) writes that mouse_trafo is equivalent to wmouse_trafo with win set to stdscr.
      (wmouse-trafo stdscr y x to-screen?)))

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
;; vim:lispwords+=auto-ptr
