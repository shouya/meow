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

## limitations & todos

nya cannot handle type check on recursive function yet. While you can
rewrite your recursive function with help of Y-combinator to support
it.

Nonetheless, I will add support for that in the following versions.


## type detection & type check
nya features type detection and type check. That means you can ask nya
"what's the type of this expression" and she will give you a proper
answer.

## nya's syntax

### commands

* `(annotate name :: type)`: annotate a name with a type signature
* `(deftype typename)`: define typename (does actually nothing, only for legibility)
* `(def name expr)`: define a variable with the value of given expression

### expressions

* `(if cond then else)`: just if, simply, cond part will be required to have type 'Bool'
* `(let var val expr)`: bind the result of val to var, and expression expr under the new binding
* `(fn :: type [params ...] body)`: define a function with type specified. the params and the body will be type checked.
* `1,2,3,...`: integer values
* `"xxxx",...`: strings
* `#t,#f`: boolean values
* `var`: any symbol is an identifier
* `(func args ...)`: apply args to func
* `(:: expr type)`: type annotated expressions, the type check of expression will be ignored and the result of the whole expression will be the given type
* `($ exprs ...)`: just like bracket-ed expressions, in order to distinct from function calling.

### type signature

* `A`: name started with uppercased letter is a regular type
* `(T/U A B C)`: union of types `A`, `B`, and `C`.
* `A -> B -> C`: function type, means that it takes values in type `A` and `B`, and return something in type `C`
* `(A)`: the same as `A`

## have fun!
