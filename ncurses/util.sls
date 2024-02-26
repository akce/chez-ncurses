;; Common ncurses public utility functions.
;; Written by Jerry 2023-2024.
;; SPDX-License-Identifier: Unlicense
(library (ncurses util)
  (export
    text-user-interface
    get-key-combo
    key-combo-alt?
    key-combo-key
    init-colour-24
    )
  (import
    (chezscheme)
    (except (ncurses) box meta))

  ;; Safe wrapper for fullscreen ncurses apps.
  (define-syntax text-user-interface
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

  (define get-ncint
    (cond
      [(foreign-entry? "wget_wch")
       wget-wch]
      [(foreign-entry? "wgetch")
       wgetch]
      [else
        ;; This case should never happen.
        ;; By successfully importing (ncurses), either the widechar enabled, or
        ;; byte-char libncurses should be loaded.
        (error 'get-charint "ncurses: neither wget_wch or wgetch found.")]))

  ;; Double negatives do my head in, define such that:
  ;;   > (blocking? win) => #t
  ;; is the same as
  ;;   > (is-nodelay win) => #f
  ;;
  ;; is_nodelay() exists only if ncurses is built with NCURSES_OPAQUE.
  ;; Its not being available is a real possibility and the case for at least one platform.
  (define blocking?
    (cond
      [(foreign-entry? "is_nodelay")
       (lambda (win)
         (not (is-nodelay win)))]
      [else
        (lambda (win-ignored)
          ;; Assume the default value of !nodelay and return that.
          ;; TODO provide a work around, eg. write a wrapper for (nodelay win) that caches state.
          #t)]))

  ;; getch-now: quickly get the next key, or #f if none.
  ;;
  ;; This function temporarily puts `getch` in non-blocking mode and tries to get the next key.
  ;; The key is returned if available, otherwise #f is returned.
  ;;
  ;; This function can help reading ALT key combos as they are sent as KEY_ESCAPE followed by the key.
  ;; A pressed ESCAPE on its own has nothing following and is seen after ESCDELAY milliseconds.
  ;;
  ;; NOTE: This assumes that nodelay is disabled by the calling app. ie, ncurses is in BLOCKING mode.
  (define getch-now
    (case-lambda
      [()
       (getch-now stdscr)]
      [(win)
       ;; This function works by temporarily putting ncurses in non-blocking mode via nodelay.
       (let ([get-symchar
               (lambda ()
                 ;; Return #f if ESCDELAY timeout expires without a key press.
                 ;; Note that our getch wrappers raise an error rather than return ERR.
                 (guard (e [else #f])
                   (key-symchar (get-ncint win))))])
         (cond
           [(blocking? win)
            (dynamic-wind
              (lambda ()
                ;; blocking => #f
                (nodelay win #t))
              get-symchar
              (lambda ()
                ;; blocking => t
                (nodelay win #f)))]
           [else
             (get-symchar)]))]))

  (define get-key-combo
    (case-lambda
      [()
       (get-key-combo stdscr)]
      [(win)
       (let ([key-a (key-symchar (get-ncint win))])
         (case key-a
           [(KEY_ESCAPE)
            (cond
              [(getch-now win)
               => (lambda (key)
                    (cons 'ALT key))]
              [else
                (cons #f key-a)])]
           [else
             (cons #f key-a)]))]))

  ;; Accessors for get-key-combo return.
  (define key-combo-alt?
    (lambda (key-combo)
      (eq? (car key-combo) 'ALT)))
  (define key-combo-key cdr)

  (define init-colour-24
    (lambda (colour-num rrggbb)
      (apply init-color colour-num (rrggbb->ncurses rrggbb))))

  (define rrggbb->ncurses
    (lambda (rrggbb)
      (map
        octet->ncurses
        (split-rrggbb rrggbb))))

  ;; Ncurses colours are between 0 to 1000, whereas a lot of apps use 0 to #xff.
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
