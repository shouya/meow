#lang racket/base

(require racket/match)
(require racket/string)

'((deftype (T/Nil  :: a))
  (deftype (T/Cons :: a -> (T/U Nil (Cons a)) -> (Cons a)))

  (typealias (List a)
             (T/U Nil (Cons a)))

  (annotate (empty? :: (List a) -> Bool))
  (annotate (cdr    :: (List a) -> (List a)))        ;; how to impl depedent type here?
  (annotate (+      :: Int -> Int -> Int))
  (annotate (if     :: Bool -> a -> a -> a))

  (defn (length :: (List a) -> Int)
    [xs]
    (if (empty? xs)
            0
            (+ 1 (length (cdr xs)))))
  )



(define (make-value type)
  (list 'val (compile-type type)))

(define (compile-type type)
  (match type
    [(list 'T/U ts ...)    (make-type-union ts)]
    [(list a)              (compile-type a)]
    [(list f '-> r ...)    (make-func-type
                            (compile-type f)
                            (compile-type r))]
    [(list tf ts ...)      (make-typefunc-type
                            tf (map compile-type ts))]
    [(? global-type-name?) (make-global-type type)]
    [(? type-var-name?)    (make-type-var type)]
    ))

(define (make-type-union ts)
  (cons 'U (compile-type ts)))

(define (global-type-name? name)
  (regexp-match #rx"^[A-Z]" (symbol->string name)))

(define (make-global-type type)
  (cons 'T type))

(define (type-var-name? name)
  (regexp-match #rx"^[a-z]" (symbol->string name)))

(define (make-type-var type)
  (cons 'TV type))

(define (make-func-type f r)
  (cons 'F (cons f r)))

(define (make-typefunc-type tf ts)
  (cons 'TF (cons tf ts)))

(define (type-of expr [ns '()])
  (match expr
    [(list f   args ...) (type-of-func-appl f args ns)]
    [(? integer? i)      (make-global-type 'Int)]
    [(? string?  s)      (make-global-type 'Str)]
    [(? boolean? b)      (make-global-type 'Bool)]
;    [(list 'fn args ...) (type-of )]
    ))

(define (tc-fail-unmatched given expected)
  (error 'type-check
         "type unmatched, \n\tgiven:\t~a\n\texpected:\t~a"
         (type->string given)
         (type->string expected)))

; (define (tc-fail-

(define (type-of-func-appl func args ns)
  (match (get-func-type func ns)
    ['() (error 'type-of-func-appl "function '~a' not found" func)]
    [(cons 'FT (cons at rett)) ;; a -> b
     (match args
       [(cons a rst) (if (is-of-type? a at ns)
                         'you-re-cool-and-preceed-the-rest   ; TODO: undefined
                         (tc-fail-unmatched (type-of a) at))]
       ['()          rett])]
    )) ; TODO: another case, pure value vs. empty args


(define (is-of-type? val type [ns '()])
  (type< (type-of val ns) type))

(define (type< t1 t2)
  undefined)  ; TODO: undefined

(define (get-func-type func-name ns)
  undefined) ; TODO: undefined

(define undefined '())


; U T TV F TF
(define (type->string type)
  (match type
    [(cons 'T t)     (symbol->string t)]
    [(cons 'TV v)    (symbol->string v)]
    [(cons 'U xs)    (string-join (map type->string xs) " + "
                                  #:before-first "("
                                  #:after-last ")")]
    [(cons 'F (cons a r))
     (string-append (type->string a)
                    " -> "
                    (type->string r))]
    [(cons 'TF (cons t ts))
     (string-join (cons (symbol->string t)
                        (map type->string ts))
                  " "
                  #:before-first "("
                  #:after-last ")")]
    ))

(compile-type '(A -> (B a)))
(type->string (compile-type '(A -> (B a))))
