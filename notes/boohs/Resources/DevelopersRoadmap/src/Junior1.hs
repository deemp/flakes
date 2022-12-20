module Junior1 (main) where

import Data.Function ((&))

-- How to declare a function? What are the function declaration parts?
-- What is the type (type signature) of a function?
main :: IO ()
main = putStrLn "Hello, world!"

-- Is it possible to declare a function without specifying its type signature? Can it cause problems, and if so, which ones?

-- But I want a Monoid
f1 :: Semigroup a => a -> a
f1 a = a <> a

-- How and when the let... in... expression is used?
f2 :: Num a => a -> a
f2 a = let b = 3 in a + b

-- How and when the where... clause is used?
f3 :: Integer
f3 = h'
 where
  h' = 3

-- Function application:

-- What is the difference between the following ways to apply a function:
-- f x (function application syntax), $ operator, & operator?
f4 :: (a -> b -> c) -> c
f4 s = t4
 where
  x = undefined :: a
  y = undefined :: b
  -- left assoc
  _ = s x
  -- left assoc
  _ = x & s
  t3 :: a -> b
  t3 _ = y
  -- right assoc
  t4 = s x $ t3 x

-- Why do they exist?
-- To apply functions in syntactically different ways

-- What is their precedence and associativity?
-- function application - left assoc, precedence 10 - https://stackoverflow.com/a/71032921
-- infixl 1 &
-- infixr 0 $

-- What is partial application?

-- What functions can be applied partially?
-- Those having at least one argument

-- Why there are no function arguments with default values in Haskell?
-- Functions should be total in arguments

-- Sectioning:
-- What is section?
-- https://wiki.haskell.org/Section_of_an_infix_operator

-- Which functions could be used in sections?
-- Any infix

-- Could you use other than binary functions in sections?
-- Yes. Due to the right associativity of function type

f5 :: a -> b -> c -> d
f5 = undefined
 where
  x = undefined :: a
  y = undefined :: b
  g = (x `f5`) y

-- Function application precedence:

-- What are the possible values for precedence in Haskell?
-- 1 - 9

-- What is associativity of operators?
-- It defines how to parenthesize the expressions using that operator
-- 3 * 4 * 5 -> (3 * 4) * 5

-- How does it differ from associativity as a mathemathical property?
-- Mathematical associativity property states that for an operator *
-- (a * b) * c = a * (b * c)

-- Is the function application syntax associative? In other words, is (a b) c identical to a (b c)?
-- No

-- What are the possible values for associativity in Haskell?
-- infixl, infixr

-- Function composition:
-- How function composition operator is declared? What are its precedence and associativity?
-- infixr 9 .

f6 :: c -> c
f6 = id . id

-- Is function composition mathematically associative?
-- Yes

f7 :: (c -> d) -> (b -> c) -> (a -> b) -> (a -> d)
f7 x y z = n1
 where
  n1 = (x . y) . z
  n2 = x . (y . z)

-- What is a tail recursion?
-- It's when the recursive call is the last statement executed by the function

-- What is a tail call optimization?
-- Turn recursion into a loop

-- Can you give examples of functions with and without tail call optimization?
f8 :: (Eq t, Num t) => t -> t
f8 0 = 0
f8 n = n + f8 (n - 1)


-- https://stackoverflow.com/a/13052612
f9 :: (Eq t, Num t) => t -> t -> t
f9 0 acc = acc
f9 n acc = f9 (n - 1) $! (acc + n)

