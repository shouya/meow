#lang racket/base

(require racket/match)
(require racket/string)

'((deftype (Nil  :: a))
  (deftype (Cons :: a -> (T/U Nil (Cons a)) -> (Cons a)))

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


(define (function-type? type)
  (eq? (car type) 'F))

(define (union-type? type)
  (eq? (car type) 'U))

(define (regular-type? type)
  (not (or (union-type? type)
           (function-type? type))))

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
    [(list f args ...) (type-of-func-appl f args ns)]
    [(? integer? i)    (make-global-type 'Int)]
    [(? string?  s)    (make-global-type 'Str)]
    [(? boolean? b)    (make-global-type 'Bool)]
;    [(list 'fn args ...) (type-of )]
    ))

(define (type-compatible? t1 t2)
  (match* (t1 t2)
    [((cons 'F a1 r1) (cons 'F a2 r2)) (and (type-compatible? a2 a1)
                                            (type-compatible? r1 r2))]
    [((cons 'U ts) _) (ormap (λ (t) (type-compatible? t t2)) ts)]
    [(_ (cons 'U ts)) (ormap (λ (t) (type-compatible? t1 t)) ts)]
    [


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


(define (union-type< type types ns tvns)
  (define uts types)
  (define (any<uts t)
    (ormap (λ (x) (type< t x ns tvns)) uts))
  (match type
    [(cons (or 'T 'TV 'F 'TF) _)
     (any<uts type)]
    [(cons 'U uts-subset)
     (andmap (λ (t) (any<uts t)) uts-subset)]))


;; clojure-mixed assoc func, like assoc but replace/add values
(define (clojure-assoc pairs key val)
  (match pairs
    ['() (list (list key val))]
    [(cons (list k v) xs)
     (if (equal? k key)
         (cons (list key val) xs)
         (cons (list k v) (clojure-assoc xs key val)))]))


(define (typevar-type< type varname ns tvns)
; (let ([new-tvns (clojure-assoc tvns varname t1)])
  (if (unbound-typevar? varname tvns)
      #t
      (type< type (typevar-deref varname tvns) ns tvns)))


(define (global-type< type typename ns tvns)
  (cond
   [(eq? (car type) 'TV)
    (if (unbound-typevar? (cdr type) tvns)
        #f
        (let ([force-t1 (typevar-deref (cdr type) tvns)]
              [t2       (make-global-type typename)])
          (type< force-t1 t2 ns tvns)))]
   [(eq? (car type) 'T) (equal? t (cdr type))]
   [#t                  #f]))


(define (force-type type tvns)
  (if (eq? (car type) 'TV)
      (typevar-deref (cdr type) tvns)
      type))

;; name space:

; U T TV F TF
(define (type< t1 t2 [ns '()] [tvns '()])
  (match t2
    [(cons 'U ts) (union-type< t1 ts ns tvns)]
    [(cons 'TV varname) (typevar-type< t1 varname ns tvns)]
    [(cons 'T t) (global-type< t1 t ns tvns)]
    [(cons 'F arg rst) (func-type< t1 t2
    ;; TODO: F, TF
    ))








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
