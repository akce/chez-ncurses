#! /usr/bin/chez-scheme --program

;; Example ncurses app.
;; It:
;; - prints the various keycode info for pressed keys
;; - shows the window dimensions
;; - displays the various ACS characters.

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

(mvaddch 3 0 ACS_ULCORNER) (addstr " ACS_ULCORNER")
(mvaddch 4 0 ACS_LLCORNER) (addstr " ACS_LLCORNER")
(mvaddch 5 0 ACS_URCORNER) (addstr " ACS_URCORNER")
(mvaddch 6 0 ACS_LRCORNER) (addstr " ACS_LRCORNER")
(mvaddch 7 0 ACS_LTEE) (addstr " ACS_LTEE")
(mvaddch 8 0 ACS_RTEE) (addstr " ACS_RTEE")
(mvaddch 9 0 ACS_BTEE) (addstr " ACS_BTEE")
(mvaddch 10 0 ACS_TTEE) (addstr " ACS_TTEE")
(mvaddch 11 0 ACS_HLINE) (addstr " ACS_HLINE")
(mvaddch 12 0 ACS_VLINE) (addstr " ACS_VLINE")
(mvaddch 13 0 ACS_PLUS) (addstr " ACS_PLUS")
(mvaddch 14 0 ACS_DIAMOND) (addstr " ACS_DIAMOND")
(mvaddch 15 0 ACS_CKBOARD) (addstr " ACS_CKBOARD")
(mvaddch 16 0 ACS_DEGREE) (addstr " ACS_DEGREE")

(mvaddch 3 16 ACS_PLMINUS) (addstr " ACS_PLMINUS")
(mvaddch 4 16 ACS_BULLET) (addstr " ACS_BULLET")
(mvaddch 5 16 ACS_LARROW) (addstr " ACS_LARROW")
(mvaddch 6 16 ACS_RARROW) (addstr " ACS_RARROW")
(mvaddch 7 16 ACS_DARROW) (addstr " ACS_DARROW")
(mvaddch 8 16 ACS_UARROW) (addstr " ACS_UARROW")
(mvaddch 9 16 ACS_BOARD) (addstr " ACS_BOARD")
(mvaddch 10 16 ACS_LANTERN) (addstr " ACS_LANTERN")
(mvaddch 11 16 ACS_BLOCK) (addstr " ACS_BLOCK")
(mvaddch 12 16 ACS_LEQUAL) (addstr " ACS_LEQUAL")
(mvaddch 13 16 ACS_GEQUAL) (addstr " ACS_GEQUAL")
(mvaddch 14 16 ACS_PI) (addstr " ACS_PI")
(mvaddch 15 16 ACS_NEQUAL) (addstr " ACS_NEQUAL")
(mvaddch 16 16 ACS_STERLING) (addstr " ACS_STERLING")

;;   ACS_S1 ACS_S9
;;   ACS_S3 ACS_S7


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
