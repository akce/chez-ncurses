#! /usr/bin/env -S chez-scheme --program

(import
 (rnrs)
 (only (chezscheme) format logtest)
 (ncurses))

(setlocale LC_ALL "")
(initscr)
(cbreak)
(mvaddstr 0 2 "Hello World")
(refresh)
(let loop ([ch (getch)])
  (cond
    [(= ch KEY_ESCAPE) (endwin)]
    [else (loop (getch))]))
