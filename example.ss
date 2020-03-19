#! /usr/bin/chez-scheme --program

;; Example ncurses app.
;; It prints the various keycode info for pressed keys and shows the window dimensions.

;; Chez-ncurses is only a thin wrapper around the ncurses library so it should be possible
;; to learn how this app works by using the ncurses man pages.

;; Written by Akce 2019-2020.
;; SPDX-License-Identifier: Unlicense

(import
 (rnrs)
 (only (chezscheme) format)
 (ncurses))

(define show-COLSxLINES
  (lambda ()
    (mvaddstr 1 0 (format "window (cols x lines): ~d x ~d" COLS LINES))))

;; Initialise ncurses.
(define win (initscr))
(assert (equal? win stdscr))

;; Turn on keypad so that KEY_RESIZE events are sent.
;; See curs_initscr(3X)
(keypad stdscr #t)

(noecho)
(cbreak)
(start-color)
(curs-set 0)
(use-default-colors)

;; Write the headers.
(mvaddstr 0 0 "press q to quit")
(show-COLSxLINES)

;; Redraw the screen and wait for user input.
(refresh)

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
         (mvaddstr 2 0 (format "key pressed: #o~o ~d #x~x ~c" ch ch ch (integer->char ch)))
         (clrtoeol)])
     (refresh)
     (loop (getch)))))

(endwin)
