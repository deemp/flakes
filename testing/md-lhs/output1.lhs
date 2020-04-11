<h1>Monoid reduction parsing/execution</h1>

I gave our apprentice the excersise to write a program 
that takes strings in the form 

< "1+4+2+3"

with variable length and write it in way that it is easily
extendible to strings of the form

< "1*2*3*4"

Not a hugely interesting task normally.
However, after giving the task, it occurred to me, that
the examples involved evaluating Monoids, specifically 
the product and sum monoid on natural numbers.

This got me thinking:
What would be an elegant, extendible way to write such a 
program?

This is what I came up with:

I am using these language extensions, I will explain their usage
when it comes up.

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}

Next, let's import the needed modules:

> module Main where
> import Data.Monoid          -- obviously when we are talking monoids 
> import Data.List            -- an interesting monoid, and some helper functions


<h2>What we want to have</h2>

What is is exactly what we want to have? We want something
that expresses an idea for reducing a monoid string (a string
that expresses a monoid computation) that generalizes over 
any monoid we throw at it.

In Haskell we use a typeclass for that:

> class (Monoid (m a)) => Reducible m a where
>         op     :: m a -> Char
>         constr :: m a -> a -> m a
>         get    :: m a -> m a -> a

This states that a `Reducible` has to be a `Monoid`
and we would like to konw what `Char` `op`erand it is associated with.
Additioally, we would like to know how to construct the `Monoid` form 
a value and a way to extract a value from the `Monoid`.
Here `MultiParamTypeClasses` enables us to writd `Reducible m a` 
so that monoids with different base types can behave differently.

Now that we have written down what we want, we can try to implement 
a `Reducible`. To begin, we will start with the `Sum` and `Product`
monoids mentioned in the beginning:

> instance (Num a) => Reducible Sum a where
>         op     _ = '+'
>         constr _ = Sum
>         get    _ = getSum
> 
> instance (Num a) => Reducible Product a where
>         op     _ = '*'
>         constr _ = Product
>         get    _ = getProduct

As you might have noticed, we completely ignore the given argument in all
three functions. The argument is merely used as a phantom type, for 
the compiler to disambiguate what instance (and with it, which dictionary) 
will be used at runtime.
The `FlexibleInstances` extension allows us to specify an instance with a 
`Num` constraint, to let us use the two instances with all `Num` instances.

<h2>How do we work with this?</h2>
Let's define how we actually reduce a "monoid string" to the value it 
represents.

The function `reduce'` takes a string and the information what monoid 
the string represents and calculates the value from it:

> reduce' :: (Read a, Reducible m a) => String -> m a -> a 

We need the Read a to make sure, that we can actually parse the values
the monoid holds. It would be nicer and safer to use a proper parser here
but for the sake of simplicity we keep it like that.
(We can always wrap a `pureTry` around the call to `reduce'`, which, again, 
is not a nice complete way to do it, but easier. ^^)
Now let's drop reduce's curtain of uncertainty:

> reduce' list red = get red  
>                 $ foldr (<>) mempty
>                 $ map (\x -> constr red $ read x) 
>                 $ splitOn (op red) list 

What is going on here?
It's actually quite simple:
First, we split the string (`list`) by the reducibles operator.
Then we convert each value in the resulting list to the monoid itself.
This leaves us with a list of monoid values, that we can reduce with a
fold and the monoid operation `<>` and finally we `get` the value
out of the monoid, using the reducibles info on how to do that.

The `splitOn` function itself is rather boilerplate:

> splitOn :: (Eq a) => a -> [a] -> [[a]]
> splitOn _ [] = [[]]
> splitOn c (h:t) 
>   | h == c    = []:rest
>   | otherwise = (h:(head rest)):(tail rest)
>    where rest = splitOn c t 

no funny business here.

<h2>However...</h2>
... we would like an interface that is a bit simpler.
Wouldn't it be nice, to write 

< reduce "1+2+3+4"

and get back the `Int` it computes to?

Let's write such a function:

> reduce :: String -> Int

The type now restricts us to returning `Int`s, but we could easily write 
another function that returns strings, etc.
(We could even use TemplateHaskell or something along the `Typeable` and 
`Data` typeclasses to automatically write such functions or return polymorphic
results and work with them, but that is something for another post.)
 

> reduce s
>     | '+' `elem` s = reduce' s (mempty :: Sum Int)
>     | '*' `elem` s = reduce' s (mempty :: Product Int)
>     | ':' `elem` s = reduce' s (mempty :: [Int])
>     | otherwise    = error "no calculatable operation detected"

The implementation is actually rather straight forward:
we just peek into the string what operand we find and then make a call to
`reduce'` with the proper monoid type.
Probably you have found the `'.'` by now and wonder if you missed something 
up until now.
Don't fear, you haven't.
I have saved somthing interesting for the end.
You probably konw that Lists also have a monoid instance in Haskell.
Therefore, we can reasonably write an instance for `Reducible`saved somthing interesting for the end.
You probably konw that Lists also have a monoid instance in Haskell.
Therefore, we can reasonably write an instance for `Reducible`: 

> instance Reducible [] Int where
>   op     _ = ':'
>   constr _ = (:[])
>   get    _ = sum -- length -- basically all [a] -> Int Functions

which even generalizes the behavior from above.
Note that the reduction part has now moved to the `get` function instead of
the binary monoid operation `<>`.
What is this good for you ask?
Monoids are bound by certain laws<sup id="laws">[1](#fn_laws)</sup>.

< -- Identity laws
< x <> mempty = x
< mempty <> x = x
< 
< -- Associativity
< (x <> y) <> z = x <> (y <> z)

These state that the binary operation needs to be associative (so we couldn't have
a monoid operation for division for example, as this would violate associativity.
However, moving the reduction operation to the reducible instead, we can specify
a function that is not associative by just implementing a function from 

< [Int] -> Int

For example:

< foldl (/) 1 list

or even have a non binary function such as 

< f l = func l True
<   where
<     func []       _     = []
<     func (h:[])   _     = h
<     func (x:y:ls) True  = x*y + (func ls False)
<     func (x:y:ls) False = x/y - (func ls True)

which is sensitive to where in the list the operands are.
 
As an additional example this is how one could implement string concattenation 
using a reducible:
 

> instance Reducible [] String where
>   op     _ = '.'
>   constr _ = (:[])
>   get    _ = intercalate [] 


<h2>Final Words</h2>
The final example along with the ability to shift the reduction step to the 
reducible opens up insersting applications for implementing simple DSL computations
or just fast, easily understandable parsers for complex data structures.
(Like implementing a `Reducible [] Foo` where Foo is some complex data type.) 

> main = do
>         print $ reduce "1+2+3+4"
>         print $ reduce "2*3*4"
>         print $ reduce "2:3:4" 
>         print $ reduce' "\"hello \".\"world\"" (mempty :: [String])

---------------------------------------------------------------------------------------
<b name="fn_laws">1</b>: These are lazily taken from the [Haskell Website](https://wiki.haskell.org/Monoid)[<-](#laws)
