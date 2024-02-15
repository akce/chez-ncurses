;; Chez Scheme ncurses common internal functions.
;; Written by Jerry 2019-2024.
;; SPDX-License-Identifier: Unlicense

(library (ncurses common)
  (export
    ERR OK
    errok
    window
    window*
    chtype

    ncurses-error ncurses-error?

    auto-ptr
    c_funcs
    enum
    lambda-errok
    lambda-name
    lambda-nullptr
    symbol->curses-name
    )
  (import
    (rename (except (chezscheme) box) (meta %meta)))

  (define-syntax enum
    (syntax-rules ()
      [(_ (col val) ...)
       (begin
         (define col val) ...)]))

  (enum
   (ERR	-1)
   (OK	0))

  (define-ftype errok int)
  (define-ftype window (struct))
  (define-ftype window* (* window))
  (define-ftype chtype unsigned)

  ;; Deriving from &error means that (error? ncurses-error) => #t
  ;; We could use &serious instead so that these exceptions are distinguishable.
  (define-condition-type &ncurses-error &error make-ncurses-error ncurses-error?)

  ;; ncurses error conditions are like standard &error ones, except &message is optional.
  ;; Including a continuation condition allows for debugging via the Chez Scheme debugger
  ;; otherwise we'd get an error message of "the raise continuation is not available".
  (define ncurses-error
    (case-lambda
      [(w)
       (call/cc
         (lambda (k)
           (raise
             (condition
               (make-ncurses-error)
               (make-who-condition w)
               (make-continuation-condition k)))))]
      [(w msg . irritants)
       (call/cc
         (lambda (k)
           (raise
             (condition
               (make-ncurses-error)
               (make-who-condition w)
               (make-message-condition msg)
               (make-irritants-condition irritants)
               (make-continuation-condition k)))))]))

  (define-syntax auto-ptr
    (syntax-rules ()
      [(_ ((var type) ...) body body* ...)
       (let ([var (make-ftype-pointer type (foreign-alloc (ftype-sizeof type)))] ...)
         (dynamic-wind
           (lambda () #f)
           (lambda () body body* ...)
           (lambda ()
             (foreign-free (ftype-pointer-address var)) ...)))]))

  (%meta begin

    ;; #t: generate code that checks 'errok' return and raises exception on ERR.
    ;; #f: ignore return and assume the caller will check.
    (define check-return? #t)

    (define ftype-ptr-return?
      (lambda (x)
        (syntax-case x (*)
          [(* field)
           (identifier? #'field)
           #t]
          [_
            #f])))

    (define string-map
      (lambda (func str)
        (list->string (map func (string->list str)))))

    (define symbol->curses-name
      (lambda (sym)
        (string-map (lambda (c)
                      (if (eqv? c #\-)
                        #\_
                        c))
                    (symbol->string sym))))
    )

  ;; A lambda wrapper that checks body return code for ERR and raises exception.
  ;; An extra name arg is used to help distinguish the exception in stack traces.
  (define-syntax lambda-errok
    (syntax-rules ()
      [(_ name (args ...) body ...)
       (lambda (args ...)
         (let ([rc (begin
                     body ...)])
           (cond
             [(fx=? rc ERR)
              (ncurses-error 'name (list args ...))]
             [else
               rc])))]))

  ;; Plain lambda that ignores the name arg.
  ;; Use in place of lambda-errok when that check isn't needed.
  (define-syntax lambda-name
    (syntax-rules ()
      [(_ name args body ...)
       (lambda args body ...)]))

  ;; Raise ncurses-error on NULL ptr return.
  (define-syntax lambda-nullptr
    (syntax-rules ()
      [(_ name (args ...) body ...)
       (lambda (args ...)
         (let ([fptr (begin
                       body ...)])
           (cond
             [(ftype-pointer-null? fptr)
              (ncurses-error 'name (list args ...))]
             [else
               fptr])))]))

  (define-syntax c_funcs
    (lambda (stx)
      (define has-chtype?
        (lambda (args)
          (find
            (lambda (x)
              (eq? 'chtype x))
            (syntax->datum args))))
      (syntax-case stx ()
        [(_ (name (types ...) return))
         (or (has-chtype? #'(types ...))
             (and (identifier? #'return)
                  (eq? 'errok (syntax->datum #'return)))
             (ftype-ptr-return? #'return))
         ;; For ncurses functions with a chtype argument, generate a calling function
         ;; that allows for those args to take either an int (as ncurses expects), or
         ;; a scheme char object.
         ;; The interface still needs to accept INTs because not all ncurses ints can
         ;; be converted to a unicode char. eg, the ACS_* chars.
         (with-syntax ([func-string (symbol->curses-name (syntax->datum #'name))]
                       [((arg arg->int) ...)
                        (map
                          (lambda (t a)
                            (list
                              a
                              (if (eq? 'chtype t)
                                  ;; 'a' is already a syntax object, so it needs to be
                                  ;; inserted into the return code syntax object as is.
                                  #`(if (char? #,a)
                                        (char->integer #,a)
                                        #,a)
                                  a)))
                          (syntax->datum #'(types ...))
                          (generate-temporaries #'(types ...)))]
                       [my-lambda
                         (cond
                           [(and check-return?
                                 (ftype-ptr-return? #'return))
                            (datum->syntax #'name 'lambda-nullptr)]
                           [(and check-return?
                                 (eq? 'errok (syntax->datum #'return)))
                            (datum->syntax #'return 'lambda-errok)]
                           [else
                             (datum->syntax #'return 'lambda-name)])])
           #'(define name
               (let ([f (foreign-procedure func-string (types ...) return)])
                 (my-lambda name (arg ...)
                   (f arg->int ...)))))]
        [(_ (name args return))
         #`(define name
             (foreign-procedure #,(symbol->curses-name (syntax->datum #'name)) args return))]
        [(_ f ...)
         #'(begin
             (c_funcs f) ...)])))
  )
;; vim:lispwords+=auto-ptr,%meta,my-lambda
