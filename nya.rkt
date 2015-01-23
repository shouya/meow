#lang racket/base



'((deftype (T/Nil  :: a))
  (deftype (T/Cons :: a -> (T/U T/Nil (T/Cons a)) -> T/Cons a))

  (typealias (T/List a)
             (T/U T/Nil (T/Cons a)))

  (annotate (empty? :: (T/List a) -> T/Bool))
  (annotate (cdr    :: (T/List a) -> (T/List a)))        ;; how to impl depedent type here?
  (annotate (+      :: T/Int -> T/Int -> T/Int))
  (annotate (if     :: T/Bool -> a -> a -> a))

  (defn (length :: (T/List a) -> T/Int)
    [xs]
    (if (empty? xs)
            0
            (+ 1 (length (cdr xs)))))
  )
