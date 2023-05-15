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
(define MOUSE_MASK_ROW 2)
(define EVENT_INFO_ROW 3)
(define ACS_INFO_ROW 4)
;; Not all terms support REPORT_MOUSE_POSITION or have it enabled if they do.
;; ie, you may need to turn it on via some config or even use VT codes or similar.
(define MOUSE_EVENT_MASK (bitwise-ior BUTTON1_CLICKED BUTTON1_DOUBLE_CLICKED BUTTON2_CLICKED BUTTON3_CLICKED REPORT_MOUSE_POSITION))

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
      (mvaddstr MOUSE_MASK_ROW 1 (format "mouse mask: requested #x~x actual #x~x old #x~x~n" MOUSE_EVENT_MASK actual-mask old-mask)))))

;; Draw the static (non-changing) screen elements.
(define example-screen-draw-static
  (lambda ()
    (box stdscr ACS_VLINE ACS_HLINE)

    ;; Write the headers.
    (mvaddch 0 1 ACS_RTEE)
    (mvaddstr 0 2 "press q or double click left mouse button to quit")
    (addch ACS_LTEE)
    (show-COLSxLINES)

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

;; Running through `dynamic-wind` ensures that `endwin` is called and the screen restored, even on catastrophic failure.
(dynamic-wind
  example-ncurses-init
  
  (lambda ()
    (example-screen-draw-static)

    ;; Main event loop.
    (call/cc
      (lambda (break)
        (let loop ([ch (getch)])
          (cond
            [(= ch (char->integer #\q))
             (break)]
            [(= ch KEY_RESIZE)	; window size has changed.
             (show-COLSxLINES)]
            [(= ch KEY_MOUSE)
             ;; Give handle-mouse-event `break` as it may decide to end the event loop too.
             (handle-mouse-event break)]
            [else
              (mvaddstr EVENT_INFO_ROW 1 (format "key pressed: #o~o ~d #x~x ~c" ch ch ch (integer->char ch)))
              (clear-to-row-end EVENT_INFO_ROW)])
          (refresh)
          (loop (getch))))))

  endwin)
