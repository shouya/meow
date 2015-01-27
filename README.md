# nya

## nya the catgirl

![nya](http://i.imgur.com/UcqXJyPl.jpg)<br>
a photo of nya-chan (taken by [SciFi](http://konachan.net/post/show/191401/animal-animal_ears-black_hair-cat-catgirl-cat_smil))

nya is an adorable catgirl who lives in a wonderful world paralleled
with us called にじげん. She's cute, smart, and always curious about
the world she lives. "Ahhh? Nya-chan want to check its type nyaaa~" is
what she speak most.

nya, strictly speaking, is not a programming language. Because she
does not aim to be anything executable. In the other word, I didn't
even plan to write an evaluator for her.

nya is a field for me to carry out my own idea about how a
contract-based static type-checking system should be.

nya consists of a simple dynamic-typed language and a type system for
the language.

お楽しみに!

## features

Kinds of types (will be) supported:

* regular types (T): `Int, Str, Bool`
* union types (U): `(T/U Int Str)`
* function types (F): `(Str -> Str)`

Currently only above will be implemented, because I failed on the
trial implementing these features myself:

* type parameters: `(Array Int)`
* polymorphic types: `(Array a -> Int)`
* ~~type functions~~

However, these two features will still be expected in the future
versions, perhaps after I know more about contract-based type systems.

Some other features:

* custom types (T)<sup>[1]</sup>: `(deftype Array)`

<sup>[1]</sup>: custom types are quite the same as regular types,
except the latter ones are built-in.


## type check and type inference

nya features type inference, and it works good on recursive functions
and branching expressions. e.g.

```racket
(type-of '(define len
            (fn [xs] (if (empty? xs)
                       0
                       (+ 1 (len (cdr xs)))))))
; => '(Array -> Int)

(type-of '(if #t 100 "str")) ; => '(T/U Int Str)
```
<sup>*</sup> above code is just for demonstration. the `type-of`
  function is only used internally. However, I will expose an
  interface for the users to acquire the type of an expression.


nya could also check for type conflicts:
```racket
(check '(fn [x] (if x (+ x x) x)))
; => error: Int is incompatible with Bool

(check '(+ #f #t))
; => error: Int expected, Bool given
```
