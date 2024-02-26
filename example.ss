#! /usr/bin/env -S chez-scheme --debug-on-exception --script

;; Example ncurses app, it:
;; - prints the keycode info for pressed keys
;; - shows the window dimensions
;; - displays the ACS characters
;; - responds to some mouse button clicks
;; - use of separate newwin for event info.

;; chez-ncurses is only a thin wrapper around the ncurses library so it should be possible
;; to learn how this app works by using the ncurses man pages.

;; Written by Jerry 2019-2024.
;; SPDX-License-Identifier: Unlicense

(import
 (rnrs)
 (only (chezscheme) format ftype-pointer=? logtest)
 (ncurses)
 (only (ncurses util) get-key-combo key-combo-alt? key-combo-key))

(define event-win #f)

(define WIN_INFO_ROW 1)
(define COLOUR_INFO_ROW 2)
(define MOUSE_MASK_ROW 3)
(define EVENT_INFO_ROW 4)
(define ACS_INFO_ROW 5)
;; Not all terms support REPORT_MOUSE_POSITION or have it enabled if they do.
;; ie, you may need to turn it on via some config or even use VT codes or similar.
(define MOUSE_EVENT_MASK (bitwise-ior BUTTON1_CLICKED BUTTON1_DOUBLE_CLICKED BUTTON2_CLICKED BUTTON3_CLICKED REPORT_MOUSE_POSITION))
(define mouse-actual-mask #f)
(define mouse-old-mask #f)

(define use-colour? #f)
(define STYLE_BORDER 44)
(define STYLE_LABEL 45)
(define STYLE_TEXT 46)

(define example-ncurses-init
  (lambda ()
    (setlocale LC_ALL "")

    ;; Initialise ncurses.
    (let ([win (initscr)])
      ;; Storing `win` here is not needed, it's only done to show that `initscr` will
      ;; initialise `stdscr` as a side-effect.
      (assert (ftype-pointer=? win stdscr)))

    ;; Turn on keypad so that KEY_RESIZE events are sent.
    ;; See curs_initscr(3X)
    (keypad stdscr #t)

    (noecho)
    (cbreak)
    (start-color)
    (curs-set 0)
    (use-default-colors)
    (let-values ([(actual-mask old-mask) (mousemask MOUSE_EVENT_MASK)])
      (set! mouse-actual-mask actual-mask)
      (set! mouse-old-mask old-mask))
    (set! event-win (newwin 1 (fx- COLS 2) EVENT_INFO_ROW 1))))

(define-syntax with-attr
  (syntax-rules ()
    [(_ (attr pair) body body* ...)
     (with-attr (stdscr attr pair) body body* ...)]
    [(_ (win attr pair) body body* ...)
     (let-values ([(old-attr old-colour) (wattr-get win)])
       (dynamic-wind
         (lambda () #f)
         (lambda ()
           (cond
             [use-colour?
               (wattr-set win attr pair)
               (let-values ([(newa newc) (wattr-get win)])
                 (assert (fx=? newa attr))
                 (assert (fx=? newc pair)))]
             [else
               ;; Call without setting colour.
               ;; This allows for setting attributes (like A_REVERSE) only.
               (wattr-set win attr)])
           body
           body* ...)
         (lambda ()
           (wattr-set win old-attr old-colour))))]))

;; Draw the static (non-changing) screen elements.
(define example-screen-draw-static
  (lambda ()
    (with-attr (A_NORMAL STYLE_BORDER)
      (box stdscr ACS_VLINE ACS_HLINE))

    ;; Write the headers.
    (mvaddch 0 1 ACS_RTEE)
    (with-attr (A_BOLD STYLE_LABEL)
      (mvaddstr 0 2
        "Press ALT-q or double click left mouse button to quit"))
    (with-attr (A_NORMAL STYLE_TEXT)
      (addch ACS_LTEE)
      (show-COLSxLINES)
      (show-colour-info)
      (mvaddstr MOUSE_MASK_ROW 1
        (format "mouse mask: requested #x~x actual #x~x old #x~x"
          MOUSE_EVENT_MASK mouse-actual-mask mouse-old-mask)))

    (let-syntax
      ([draw-column
         (syntax-rules ()
           [(_ y-start x acs-char ...)
            (let loop ([y y-start]
                       [chvals (list acs-char ...)]
                       [chstrs (list (symbol->string 'acs-char) ...)])
              (unless (null? chvals)
                (mvaddch y x (car chvals))
                (addstr (string-append " " (car chstrs)))
                (loop (fx+ y 1) (cdr chvals) (cdr chstrs))))])])
      ;; left column
      (draw-column
        ACS_INFO_ROW 1
        ACS_ULCORNER
        ACS_LLCORNER
        ACS_URCORNER
        ACS_LRCORNER
        ACS_LTEE
        ACS_RTEE
        ACS_BTEE
        ACS_TTEE
        ACS_HLINE
        ACS_VLINE
        ACS_PLUS
        ACS_DIAMOND
        ACS_CKBOARD
        ACS_DEGREE)
      ;; right column
      (draw-column
        ACS_INFO_ROW 17
        ACS_PLMINUS
        ACS_BULLET
        ACS_LARROW
        ACS_RARROW
        ACS_DARROW
        ACS_UARROW
        ACS_BOARD
        ACS_LANTERN
        ACS_BLOCK
        ACS_LEQUAL
        ACS_GEQUAL
        ACS_PI
        ACS_NEQUAL
        ACS_STERLING))

    ;;   ACS_S1 ACS_S9
    ;;   ACS_S3 ACS_S7
    ))

(define show-COLSxLINES
  (lambda ()
    (mvaddstr WIN_INFO_ROW 1
      (format "window (cols x lines): ~d x ~d" COLS LINES))))

(define show-colour-info
  (lambda ()
    (mvaddstr COLOUR_INFO_ROW 1
      (format "Colours ~d Pairs ~d has-colours? ~a can-change-colour? ~a"
        COLORS COLOR_PAIRS (has-colors) (can-change-color)))))

(define handle-mouse-event
  (lambda (break)
    (call-with-mouse-event
      (lambda (mevent*)
        (define event-mask (mevent-bstate mevent*))
        (cond
          [(logtest event-mask BUTTON1_DOUBLE_CLICKED)
           (break)]
          [(logtest event-mask MOUSE_EVENT_MASK)
           (werase event-win)
           (mvwaddstr event-win 0 0
             (format "mouse pressed: id #x~x row ~d column ~d bstate #x~x"
               (mevent-id mevent*) (mevent-y mevent*) (mevent-x mevent*) (mevent-bstate mevent*)))]
          [else
            (werase event-win)
            (mvwaddstr event-win 0 0
              (format "mouse: unknown event #x~x"
                (mevent-bstate mevent*)))])))))

(define show-event-key-combo
  (lambda (key-combo)
    (define keycode (key-ncint (key-combo-key key-combo)))
    (werase event-win)
    (with-attr (event-win A_REVERSE STYLE_TEXT)
      (mvwaddstr event-win 0 0
        (format "~a pressed: #o~o #d~d #x~x ~s"
          (if (key-combo-alt? key-combo)
            "alt-key"
            "key")
          keycode keycode keycode (key-combo-key key-combo))))))

(define-syntax batch
  (syntax-rules ()
    [(_ command (args ...) ...)
     (begin
       (command args ...)
       ...)]))

;; Running through `dynamic-wind` ensures that `endwin` is called and the screen restored, even on catastrophic failure.
(dynamic-wind
  example-ncurses-init

  (lambda ()
    ;; Normal RGB Max = 100
    ;; Extended RGB Max = 32767
    (define COLOUR_NONE -1)
    (when (> COLORS 7)
      (set! use-colour? #t)
      (when (can-change-color)
        (batch init-color
          (COLOR_GREEN 682 816 682)
          (COLOR_MAGENTA 761 682 816)
          (COLOR_CYAN 878 686 569)))
      (batch init-pair
        (STYLE_TEXT COLOR_GREEN COLOUR_NONE)
        (STYLE_LABEL COLOR_MAGENTA COLOUR_NONE)
        (STYLE_BORDER COLOR_CYAN COLOUR_NONE)))
    (example-screen-draw-static)

    ;; Main event loop.
    (call/cc
      (lambda (break)
        (let loop ([key-combo (get-key-combo)])
          (cond
            [(key-combo-alt? key-combo)
             (case (key-combo-key key-combo)
               [(#\q)
                ;; ALT-q exits the loop.
                (break)]
               [else
                 (show-event-key-combo key-combo)])]
            [else
              (case (key-combo-key key-combo)
                [(KEY_RESIZE)	; window size has changed.
                 (erase)
                 (example-screen-draw-static)]
                [(KEY_MOUSE)
                 ;; Give handle-mouse-event `break` as it may decide to end the event loop too.
                 (handle-mouse-event break)]
                [else
                  (show-event-key-combo key-combo)])])
          (refresh)
          (wrefresh event-win)
          (loop (get-key-combo))))))

  endwin)

;; vim:lispwords+=batch,format,mvaddstr,mvwaddstr,with-attr
