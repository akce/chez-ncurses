;; Common ncurses posix layer.
;; Written by Jerry 2024.
;; SPDX-License-Identifier: Unlicense
;;
;; Since i've been too lazy to write a standalone posix/os library, put
;; setlocale here so that it's obvious that this is something that is OS
;; dependent.

(library (ncurses posix)
  (export
    ;; HACK: setlocale(3) should be in a posix/os lib.
    setlocale
    LC_ALL
    operating-system
    )
  (import
    (chezscheme)
    )

  ;; load-shared-object probably already accounts for this, but don't
  ;; load the lib if setlocale is already available.
  (meta-cond
    [(foreign-entry? "setlocale")
     (begin)]
    [else
     (define library-init
       (load-shared-object #f))])

  (define setlocale
    (foreign-procedure "setlocale" (int string) string))

  ;; This is such a cheat, but since there's a lot of machine types, try and work
  ;; out the OS by peeking at the machine-type symbol.
  ;;
  ;; The list of machine types were read from Chez Scheme's v10.0 release notes:
  ;; https://cisco.github.io/ChezScheme/release_notes/v10.0/release_notes.html
  (define operating-system
    (lambda ()
      (define mt-str (symbol->string (machine-type)))
      (define-syntax doh!
        (syntax-rules ()
          [(_)
           (error 'operating-system "Unknown OS" (machine-type))]))
      (case (string-ref mt-str (fx- (string-length mt-str) 2))
        [(#\l)
         'linux]
        [(#\f)
         'freebsd]
        [(#\s)
         ;; osx (for mac os), or s2 (for opensolaris)
         (case (string-ref (string-length mt-str))
           [(#\x)
            'osx]
           [(#\2)
            'opensolaris]
           [else
             (doh!)])]
        [(#\n)
         ;; nb (for netbsd), gnu (for gnu-hurd), or nt (for windows)
         (case (string-ref (string-length mt-str))
           [(#\b)
            'netbsd]
           [(#\u)
            'hurd]
           [(#\t)
            'windows]
           [else
             (doh!)])]
        [(#\o)
         'openbsd]
        [else
          'portable-bytecode])))

  (define LC_ALL
    (case (operating-system)
      [(linux)
       6]
      [(freebsd)
       0]
      [else
        (error 'LC_ALL "Unsupported OS" (machine-type))]))
  )
