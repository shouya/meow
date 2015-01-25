#lang racket/base

(require racket/match)
(require racket/string)
(require racket/set)

'((deftype List)

  (annotate (Nil    :: List))
  (annotate (Cons   :: (T/U Int Str Bool) -> List -> List)
  (annotate (empty? :: List -> Bool))
  (annotate (cdr    :: List -> List))
  (annotate (+      :: Int -> Int -> Int))
  ;; without polymorphic types, one can only impl
  ;; function like `if` in the compiler
  ;  (annotate (if     :: Bool -> a -> a -> a))

  (defn (length :: List -> Int)
    [xs]
    (if (empty? xs)
        0
        (+ 1 (length (cdr xs)))))
  )
  )


(define (compile-type type)
  (match type
    [(list 'T/U ts ...)     (make-union-type (map compile-type ts))]
    ;; right assoc prop of func type:
    ;;   A -> B -> C === A -> (B -> C)
    [(list dom '-> rng ...) (make-func-type
                            (compile-type dom)
                            (compile-type rng))]
    [(list a) (compile-type a)]
    [(? regular-type-name?) (make-regular-type type)]
    ))

(define (make-union-type ts)
  (define uniq (compose set->list list->set))
  (define (ts-flatten types)
    (match types
      ['() '()]
      [(cons (cons 'U ts) rts) (append (ts-flatten ts)
                                       (ts-flatten rts))]
      [(cons a rts)            (cons a (ts-flatten rts))]))
  (define result (uniq (ts-flatten ts)))

  (if (equal? (length result) 1)
      (car result)
      (cons 'U result)))

(define (regular-type-name? name)
  (regexp-match #rx"^[A-Z][a-z_]*$" (symbol->string name)))

(define (make-regular-type type)
  (cons 'T type))

(define (make-func-type dom rng)
  (cons 'F (cons dom rng)))

(define (func-type? type)
  (eq? 'F (car type)))

(define (func-type-domain type)
  (when (not (func-type? type))
    (error "try to acquire domain from a non-function"))
  (cadr type))
(define (func-type-range type)
  (when (not (func-type? type))
    (error "try to acquire range from a non-function"))
  (cddr type))



(define (type-of expr)
  (match expr
    [(list 'if cnd thn els) (type-of-if cnd thn els)]
    [(list 'fn prms bdy)    (type-of-fn prms bdy)]
    [(list fn args ...)     (type-of-fn-appl fn args)]
    ))



(define (type-of-if cnd thn els)
  (assert-type (type-of cnd) (make-regular-type 'Bool))
  (make-union-type (type-of thn)
                   (type-of els))
  )


(define (type-of-fn params body)
  (match params
    ['()         (error "empty arg is not supported")]
    [(list p)    (infer-type-of p body)]
    [(cons p ps) (make-func-type (infer-type-of p body)
                                 (type-of-fn ps body))]))


(define (type-of-fn-appl fn args)
  (define fn-type (type-of fn))
  (define (fn-beta-reducible? dom arg)
    (and (type-compatible? (type-of arg) dom)
         (type< (type-of arg) dom)))

  (define (recur fn-type args)
    (cond
     [(null? args) fn-type]
     [(not (func-type? fn-type)) (error "applying a value to a non-function")]
     [(not (fn-beta-reducible? (func-type-domain fn-type) (car args)))
      (error "applying an incompatible value to a function")]
     [else
      (recur (func-type-range fn-type) (cdr args))]))

  (recur fn-type args))


(define (assert-type t1 t2)
  (if (type< t1 t2) #t
      (error "type mismatch, expect ~a, given ~a."))) ;; TODO: define type<

(define (infer-type-of var body)
  (undefined))

(define (func-type-compatible? d1 r1 d2 r2)
  (and (type-compatible? d1 d2)
       (type-compatible? r2 r1))) ;; reversed!

(define (union-type-compatible? ts1 ts2) ;; i.e. ts1 is-subset-of ts2
  (andmap (λ (t1) (ormap (λ (t2) (type-compatible? t1 t2)) ts2)) ts1))


(define (type-compatible? type1 type2)
  (match* (type1 type2)
    [((cons 'T name1) (cons 'T name2)) (equal? name1 name2)]
    [(_               (cons 'T name2)) #f]
    [((cons 'U ts1)   (cons 'U ts2))   (union-type-compatible? ts1 ts2)]
    [(a               (cons 'U types))
     (ormap (λ (t) (type-compatible? a t)) types)]
    [((cons 'F (cons d1 r1)) (cons 'F (cons d2 r2)))
     (func-type-compatible? d1 r1 d2 r2)]
    [(_               (cons 'F _)) #f]))


(define (type< type1 type2)
  (undefined))


(define (undefined)
  (error "undefined"))

(define (type->string type)
  (match type
    [(cons 'T t)     (symbol->string t)]
    [(cons 'U xs)    (string-join (map type->string xs) " + "
                                  #:before-first "("
                                  #:after-last ")")]
    [(cons 'F (cons a r))
     (string-append (type->string a)
                    " -> "
                    (type->string r))]
    ))
#|
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
    [((cons 'T a) (cons 'T b)) (equal? a b)]
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
    [(cons 'F arg rst) (func-type< t1 t2)]
    ;; TODO: F, TF
    ))


(define (get-func-type func-name ns)
  undefined) ; TODO: undefined

|#

; U T F


;;; Test cases
(require rackunit)

(define t compile-type)
(check-equal? (t '(A -> B)) '(F . ((T . A) . (T . B))))
(check-equal? (t '((T/U A) -> B)) '(F . ((T . A) . (T . B))))
(check-true
 (or (equal? (type->string (t '((T/U A B) -> B)))
             "(B + A) -> B")
     (equal? (type->string (t '((T/U A B) -> B)))
             "(A + B) -> B")))


(check-true  (type-compatible? (t 'A) (t 'A)))
(check-false (type-compatible? (t 'A) (t 'B)))
(check-true  (type-compatible? (t 'A) (t '(T/U A B))))
(check-false (type-compatible? (t 'A) (t '(T/U B C))))
(check-false (type-compatible? (t '(T/U A B)) (t 'A)))
(check-true  (type-compatible? (t '(T/U A))   (t 'A)))
(check-true  (type-compatible? (t '(T/U A B)) (t '(T/U E A B C D))))
(check-true  (type-compatible? (t '(A -> B))  (t '(A -> B))))
(check-false (type-compatible? (t '(B -> A))  (t '(A -> B))))
(check-false (type-compatible? (t '((T/U A B) -> B)) (t '(A -> B))))
(check-true  (type-compatible? (t '(A -> B)) (t '((T/U A B) -> B))))
