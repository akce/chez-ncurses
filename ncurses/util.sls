;; Common ncurses public utility functions.
;; Written by Jerry 2023.
;; SPDX-License-Identifier: Unlicense
(library (ncurses util)
  (export
    textui
    get-key-combo
    key-combo-alt?
    key-combo-key
    init-colour-24
    )
  (import
    (chezscheme)
    (except (ncurses) box meta))

  ;; Safe wrapper for ncurses apps that use unbuffered key presses.
  (define-syntax textui
    (syntax-rules ()
      [(_ body body* ...)
       (dynamic-wind
         initscr
         (lambda ()
           (keypad stdscr #t)
           (noecho)
           (cbreak)
           (curs-set 0)
           (start-color)
           (use-default-colors)
           body
           body*
           ...
           )
         endwin)]))

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
             (key-ref (wgetch win))))
         (lambda ()
           (nodelay win #f)))]))

  (define get-key-combo
    (lambda ()
      (define key-a (key-symchar (getch)))
      (case key-a
        [(KEY_ESCAPE)
         (cond
           [(getch-now)
            => (lambda (key)
                 (cons #t key))]
           [else
             (cons #f key-a)])]
        [else
          (cons #f key-a)])))

  ;; Convenience accessors for get-key-combo return.
  (define key-combo-alt? car)
  (define key-combo-key cdr)

  (define init-colour-24
    (lambda (colour-num rrggbb)
      (apply init-color colour-num (rrggbb->ncurses rrggbb))))

  (define rrggbb->ncurses
    (lambda (rrggbb)
      (map
        octet->ncurses
        (split-rrggbb rrggbb))))

  ;; Ncurses colours are between 0 to 1000, whereas our colours are 0 to #xff.
  (define octet->ncurses
    (lambda (oct)
      (round (* (/ oct #xff) 1000))))

  (define split-rrggbb
    (lambda (rrggbb)
      (list
        (bitwise-arithmetic-shift-right (bitwise-and rrggbb #xff0000) 16)
        (bitwise-arithmetic-shift-right (bitwise-and rrggbb #x00ff00) 8)
        (bitwise-and rrggbb #x0000ff))))
  )
