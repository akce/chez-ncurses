#! /usr/bin/env -S chez-scheme --program

;; Example ncurses app, it:
;; - prints the keycode info for pressed keys
;; - shows the window dimensions
;; - displays the ACS characters
;; - responds to some mouse button clicks.

;; chez-ncurses is only a thin wrapper around the ncurses library so it should be possible
;; to learn how this app works by using the ncurses man pages.

;; Written by Jerry 2019-2020,2023.
;; SPDX-License-Identifier: Unlicense

(import
 (rnrs)
 (only (chezscheme) format logtest)
 (ncurses))

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
      (assert (eq? win stdscr)))

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
      (set! mouse-old-mask old-mask))))

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
               (wattr-set win attr pair)]
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
    (with-attr (stdscr A_NORMAL STYLE_BORDER)
      (box stdscr ACS_VLINE ACS_HLINE))

    ;; Write the headers.
    (mvaddch 0 1 ACS_RTEE)
    (with-attr (stdscr A_NORMAL STYLE_LABEL)
      (mvaddstr 0 2 "Press ALT-q or double click left mouse button to quit"))
    (with-attr (stdscr A_NORMAL STYLE_TEXT)
      (addch ACS_LTEE)
      (show-COLSxLINES)
      (show-colour-info)
      (mvaddstr MOUSE_MASK_ROW 1 (format "mouse mask: requested #x~x actual #x~x old #x~x~n" MOUSE_EVENT_MASK mouse-actual-mask mouse-old-mask)))

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

    ;; Redraw the screen and wait for user input.
    (refresh)))

(define show-COLSxLINES
  (lambda ()
    (mvaddstr WIN_INFO_ROW 1 (format "window (cols x lines): ~d x ~d" COLS LINES))))

(define show-colour-info
  (lambda ()
    (mvaddstr COLOUR_INFO_ROW 1 (format "Colours ~d Pairs ~d has-colours? ~a can-change-colour? ~a" COLORS COLOR_PAIRS (has-colors) (can-change-color)))))

(define clear-to-row-end
  (lambda (row)
     (clrtoeol)
     ;; redraw the vertical line that clrtoeol erased.
     (mvaddch row (- COLS 1) ACS_VLINE)))

(define handle-mouse-event
  (lambda (break)
    (call-with-mouse-event
      (lambda (mevent*)
        (define event-mask (mevent-bstate mevent*))
        (cond
          [(logtest event-mask BUTTON1_DOUBLE_CLICKED)
           (break)]
          [(logtest event-mask MOUSE_EVENT_MASK)
           (mvaddstr EVENT_INFO_ROW 1
                     (format "mouse pressed: id #x~x row ~d column ~d bstate #x~x"
                             (mevent-id mevent*) (mevent-y mevent*) (mevent-x mevent*) (mevent-bstate mevent*)))]
          [else
            (mvaddstr EVENT_INFO_ROW 1 (format "mouse: unknown event #x~x" (mevent-bstate mevent*)))])
          (clear-to-row-end EVENT_INFO_ROW)))))

(define show-event-key
  (lambda (event-type keycode)
    (with-attr (A_REVERSE STYLE_TEXT)
      (mvaddstr EVENT_INFO_ROW 1 (format "~a pressed: #o~o #d~d #x~x ~s" event-type keycode keycode keycode (key-ref keycode))))
    (clear-to-row-end EVENT_INFO_ROW)))


;; getch-now: quickly get the next key, or #f if none.
;;
;; This function temporarily puts `getch` in non-blocking mode and tries to get the next key.
;; The key is returned if available, otherwise #f is returned.
;;
;; This function can help reading ALT key combos as they are sent as KEY_ESCAPE followed by the key.
;; A pressed ESCAPE on its own has nothing following and is seen after ESCDELAY milliseconds.
;;
;; NOTE: This assumes that nodelay is disabled by the calling app. ie, `getch` is in BLOCKING mode.
(define getch-now
  (case-lambda
    [()
     (getch-now stdscr)]
    [(win)
     ;; This function works by temporarily putting wgetch in non-blocking mode via nodelay.
     (dynamic-wind
       (lambda ()
         (nodelay win #t))
       (lambda ()
         ;; Return #f if ESCDELAY timeout expires without a key press.
         ;; Note that our getch wrappers raise an error rather than return ERR.
         (guard (e [else #f])
           (wgetch win)))
       (lambda ()
         (nodelay win #f)))]))

(define-syntax change-colour
  (syntax-rules ()
    [(_ (colour r g b) ...)
     (begin
       (change-colour colour r g b)
       ...)]
    [(_ colour r g b)
     (let ([rc (init-color colour r g b)])
       (when (= rc ERR)
         (error 'init-color (format "Error changing colour: ~s" 'colour) r g b)))]))

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
        (change-colour
          (COLOR_BLUE 682 816 682)
          (COLOR_MAGENTA 761 682 816)
          (COLOR_CYAN 878 686 569)))
      (init-pair STYLE_TEXT COLOR_BLUE COLOUR_NONE)
      (init-pair STYLE_LABEL COLOR_MAGENTA COLOUR_NONE)
      (init-pair STYLE_BORDER COLOR_CYAN COLOUR_NONE))
    (example-screen-draw-static)

    ;; Main event loop.
    (call/cc
      (lambda (break)
        (let loop ([ch (getch)])
          (case (key-ref ch)
            [(KEY_ESCAPE)
             (cond
               [(getch-now)
                => (lambda (key)
                     (case (key-ref key)
                       [(#\q)
                        ;; ALT-q exits the loop.
                        (break)]
                       [else
                         (show-event-key "alt-key" key)]))]
               [else
                 ;; Escape pressed.
                 (show-event-key "key" ch)])]
            [(KEY_RESIZE)	; window size has changed.
             (erase)
             (example-screen-draw-static)]
            [(KEY_MOUSE)
             ;; Give handle-mouse-event `break` as it may decide to end the event loop too.
             (handle-mouse-event break)]
            [else
              (show-event-key "key" ch)])
          (refresh)
          (loop (getch))))))

  endwin)
