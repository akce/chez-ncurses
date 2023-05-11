#! /usr/bin/env -S chez-scheme --program

;; Example ncurses app, it:
;; - prints the keycode info for pressed keys
;; - shows the window dimensions
;; - displays the ACS characters.

;; chez-ncurses is only a thin wrapper around the ncurses library so it should be possible
;; to learn how this app works by using the ncurses man pages.

;; Written by Jerry 2019-2020,2023.
;; SPDX-License-Identifier: Unlicense

(import
 (rnrs)
 (only (chezscheme) format)
 (ncurses))

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
    (use-default-colors)))

;; Draw the static (non-changing) screen elements.
(define example-screen-draw-static
  (lambda ()
    (box stdscr ACS_VLINE ACS_HLINE)

    ;; Write the headers.
    (mvaddch 0 1 ACS_RTEE)
    (mvaddstr 0 2 "press q to quit")
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
        3 1 
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
        3 17
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
    (mvaddstr 1 1 (format "window (cols x lines): ~d x ~d" COLS LINES))))

;; Running through `dynamic-wind` ensures that `endwin` is called, and the screen restored, even on catastrophic failure.
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
            [else
              (mvaddstr 2 1 (format "key pressed: #o~o ~d #x~x ~c" ch ch ch (integer->char ch)))
              (clrtoeol)
              ;; redraw the vertical line that clrtoeol erased.
              (mvaddch 2 (- COLS 1) ACS_VLINE)])
          (refresh)
          (loop (getch))))))

  endwin)
