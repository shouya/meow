#lang racket/base

(require racket/match)
(require racket/string)
(require racket/set)

(define whooo
  '((deftype List)

  (annotate (Nil    :: List))
  (annotate (Cons   :: (T/U Int Str Bool) -> List -> List))
  (annotate (empty? :: List -> Bool))
  (annotate (cdr    :: List -> List))
  (annotate (+      :: Int -> Int -> Int))
  ;; without polymorphic types, one can only impl
  ;; function like `if` in the compiler layer
  ;  (annotate (if     :: Bool -> a -> a -> a))

  (def length
    (fn [xs]
      (if (empty? xs)
          0
          (+ 1 (length (cdr xs))))))
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

(define (regular-type? type)
  (eq? 'T (car type)))

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

(define (make-unknown-type)
  (cons '? '()))
(define (unknown-type? type)
  (eq? '? (car type)))



(define (type-of expr type-bindings)
  (match expr
    ;; primitive types
    [(? integer?)             (compile-type 'Int)]
    [(? string?)              (compile-type 'Str)]
    [(? boolean?)             (compile-type 'Bool)]

    ;; code structures
    ;; branching expr
    [(list 'if cnd thn els)   (type-of-if cnd thn els type-bindings)]

    ;; local binding expr
    [(list 'let var val expr) (type-of-let var val expr type-bindings)]

    ;; func expr
                                        ; TODO: check type as in typed-expr
    [(list 'fn ':: type prms bdy)
     (compile-type type)]

    ;; type-annotated expr
    [(list ':: expr type)     (type-of-typed-expr expr
                                                  (compile-type type)
                                                  type-bindings)]
    ;; bracket-ed expr
    [(list '$ expr ...)       (type-of expr type-bindings)]

    ;; variables
    [(? symbol? var)          (or (type-of-var var type-bindings)
                                  (error "variable not defined"))]

    [(list fn args ...)       (type-of-fn-appl fn args type-bindings)]

    ))

(define (make-type-binding expr type)
  (cons expr type))
(define (append-binding binding bindings)
  (cons binding bindings))

(define (type-of-let var val expr type-bindings)
  (let* ([val-type   (type-of val type-bindings)]
         [val-bding  (make-type-binding var val-type)]
         [new-bdings (append-binding val-bding type-bindings)])
    (type-of expr new-bdings)))

(define (type-of-typed-expr expr type bindings) type)


(define (type-of-if cnd thn els type-bindings)
  (assert-type (type-of cnd type-bindings) (compile-type 'Bool))
  (make-union-type (list (type-of thn type-bindings)
                         (type-of els type-bindings)))
  )

(define (more-loose-type t1 t2)
  (cond
   [(type-compatible? t1 t2) t2]
   [(type-compatible? t2 t1) t1]
   [else                     (tc-fail-mismatch t1 t2)]))

(define (type-of-fn params body type-bindings)
  (match params
    ['()         (type-of body type-bindings)]
    [(cons p ps)
     (let* ([dom       (undefined p body type-bindings)]
            [arg-type  (make-type-binding p dom)]
            [new-bdns  (append-binding arg-type type-bindings)]
            [expr-type (type-of-fn ps body new-bdns)])
       (make-func-type dom expr-type))]
    ))



(define (type-of-fn-appl fn args type-bindings)
  (define fn-type (type-of fn type-bindings))
  (define (fn-beta-reducible? dom arg)
    (and (type-compatible? (type-of arg type-bindings) dom)))

  (define (recur fn-type args)
    (cond
     [(null? args) fn-type]
     [(not (func-type? fn-type)) (error "applying a value to a non-function")]
     [(not (fn-beta-reducible?   (func-type-domain fn-type) (car args)))
      (error "applying an incompatible value to a function")]
     [else
      (recur (func-type-range fn-type) (cdr args))]))

  (recur fn-type args))


(define (func-type-compatible? d1 r1 d2 r2)
  (and (type-compatible? d1 d2)
       (type-compatible? r2 r1))) ;; reversed!

(define (union-type-compatible? ts1 ts2) ;; i.e. ts1 is-subset-of ts2
  (andmap (λ (t1) (ormap (λ (t2) (type-compatible? t1 t2)) ts2)) ts1))


(define (type-compatible? type1 type2)
  (match* (type1 type2)
    [((cons '? t1) _)                  #t]
                                        ;    [(_ (cons '? t1))                  #f]
    [((cons 'T name1) (cons 'T name2)) (equal? name1 name2)]
    [(_               (cons 'T name2)) #f]
    [((cons 'U ts1)   (cons 'U ts2))   (union-type-compatible? ts1 ts2)]
    [(a               (cons 'U types))
     (ormap (λ (t) (type-compatible? a t)) types)]
    [((cons 'F (cons d1 r1)) (cons 'F (cons d2 r2)))
     (func-type-compatible? d1 r1 d2 r2)]
    [(_               (cons 'F _)) #f]))

(define (type-equal? t1 t2)
  (and (type-compatible? t1 t2)
       (type-compatible? t2 t1)))


(define (type-of-var var type-bindings)
  (cond [(assoc var type-bindings) => cdr]
        [else                        (make-unknown-type)]))



(define (undefined)
  (error "undefined"))

(define (type->string type)
  (match type
    [(cons 'T t)     (symbol->string t)]
    [(cons 'U xs)    (string-join (map type->string xs) " + "
                                  #:before-first "("
                                  #:after-last ")")]
    [(cons '? _)     "?"]
    [(cons 'F (cons a r))
     (string-append (type->string a)
                    " -> "
                    (type->string r))]
    ))


(define (tc-fail-mismatch given expected)
  (error 'type-check-failure
         "type mismatch\n\tgiven:\t\t~a\n\texpected:\t~a"
         (type->string given)
         (type->string expected)))

(define (assert-type t1 t2)
  (if (type-compatible? t1 t2) #t
      (tc-fail-mismatch t1 t2)))


                                        ; Types: U T F
                                        ; Exprs: if let fn fn-appl



;;; Test
(require rackunit)


;; for demo purpose
(define (pre-defined-bindings)
  (define-syntax define-bindings
    (syntax-rules (::)
      [(define-bindings) '()]
      [(define-bindings [name :: type ...] rst ...)
       (let* ([n   (quote name)]
              [t   (compile-type (quote (type ...)))]
              [bdn (make-type-binding n t)])
         (append-binding bdn (define-bindings rst ...)))]))

  (define-bindings
    [+      :: Int -> Int -> Int]
    [empty? :: Array -> Bool]
    [cdr    :: Array -> Array]
    [length :: Array -> Int]
    ;; this func is not perfect without polymorphism support
    [car    :: Array -> Int]
    [ival   :: Int]
    [sval   :: Str]
    [bval   :: Bool]
    ))

(define-syntax check-type-match
  (syntax-rules (::)
    [(_ expr :: type ...)
     (check-true (type-equal? (type-of expr (pre-defined-bindings))
                              (compile-type (quote (type ...)))))]))

(define-syntax check-type-match-cases
  (syntax-rules (::)
    [(_ [case ...] ...)
     (begin
       (check-type-match case ...) ...)]))

(define-syntax check-type-compatible-cases
  (syntax-rules (<=: </:)
    [(_) (void)]
    [(_ [t1 <=: t2] rest ...)
     (begin
       (let ([ct1 (compile-type (quote t1))]
             [ct2 (compile-type (quote t2))])
         (check-true (type-compatible? ct1 ct2)))
       (check-type-compatible-cases rest ...))]
    [(_ [t1 </: t2] rest ...)
     (begin
       (let ([ct1 (compile-type (quote t1))]
             [ct2 (compile-type (quote t2))])
         (check-false (type-compatible? ct1 ct2)))
       (check-type-compatible-cases rest ...))]
    ))

(define-syntax check-compile-cases
  (syntax-rules (@==>)
    [(_ [type @==> compiled-type] ...)
     (begin
       (check-equal? (compile-type (quote type))
                     (quote compiled-type))
       ...)]
    ))


;; check type-compile
(check-compile-cases
 [(A -> B)         @==>   (F . ((T . A) . (T . B)))]
 [((T/U A) -> B)   @==>   (F . ((T . A) . (T . B)))]
 )


;; check type->string
(check-true (let* ([ctype    (compile-type '((T/U A B) -> B))]
                   [type-str (type->string ctype)])
              (or (equal? type-str "(B + A) -> B")
                  (equal? type-str "(A + B) -> B"))))


; a <=: b means a is compatible   with b
; a </: b means a is incompatible with b

(check-type-compatible-cases
 [A                <=:  A]
 [A                </:  B]
 [A                <=:  (T/U A B)]
 [A                </:  (T/U B C)]
 [(T/U A)          <=:  A]
 [(T/U A B)        </:  A]
 [(T/U A B)        <=:  (T/U E A B C D)]
 [(T/U E A B C D)  </:  (T/U A B)]
 [(A -> B)         <=:  (A -> B)]
 [(B -> A)         </:  (A -> B)]
 [(A -> B)         <=:  ((T/U A B) -> B)]
 [((T/U A B) -> B) </:  (A -> B)]
 [(A -> (T/U A B)) <=:  (A -> B)]
 [(A -> B)         </:  (A -> (T/U A B))]
 )


(check-type-match-cases
 ;; literal
 [1     :: Int]
 ["str" :: Str]
 [#t    :: Bool]
 [#f    :: Bool]
 ;; func and func appl
 ['+       :: Int -> Int -> Int]
 ['(+ 1)   :: Int -> Int]
 ['(+ 1 2) :: Int]

 ;; if
 ['(if #t 1 2)                :: Int]
 ['(if #f 1 "s")              :: T/U Int Str]
 ['(if #t #t (if #t 1 "str")) :: T/U Str Int Bool]

 ;; let
 ['(let a 1 a)            :: Int]
 ['(let a #t (if a 1 1))  :: Int]
 ['(let a 1  (if #t a 1)) :: Int]

 ;; fn (inference not available so far)
 ['(fn :: (Int -> Int) [a] 1)  :: Int -> Int]
 ;; this case should fail after finished function type check
 ['(fn :: (Int -> Int) [a] #t) :: Int -> Int]

 ;; typed-expr
 ['(:: 1 Str)  :: Str]
 ['(let a (:: 1 (Int -> Int)) (a 1)) :: Int]

 ;; bracketed expr
 ['($ + 1 2) :: Int]
 )
