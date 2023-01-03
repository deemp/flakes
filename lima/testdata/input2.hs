{-
# Top heading

- Only multi-line comments are supported for Markdown

- Haskell code snippet

    ```haskell
    "1+4+2+3"
    ```

- Console code snippet

    ```console
    "1*2*3*4"
    ```

1. what's in comments is written as is
2. Haskell code becomes haskell code snippets

- There should be a line between a multi-line comment and Haskell code

- A Haskell snippet starts with a non-empty line that is not a part of a multi-line comment

- Magic comments will be skipped

- Haskell function docs won't

- If there's a single line btw Haskell code and a comment, this line is removed
-}

{- FOURMOLU_DISABLE -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{- FOURMOLU_ENABLE -}

{- Some imports here -}

-- this comment will go into the Haskell snippet
-- Imports
module Main where -- a comment after

{- This comment will divide Haskell snippets -}

import Data.Monoid (Product (..), Sum (..))

{-
## Second-level heading

some text

### Third-level heading
-}

-- This comment will go into a Haskell snippet

-- | This doc will be omitted
-- and won't go into a Haskell snippet
class (Monoid (m a)) => Reducible m a where
    op :: m a -> Char

    -- | this too
    constr :: m a -> a -> m a

    -- | and this one
    get :: m a -> m a -> a

{- don't put anything outside and after this comment like in `{- -} -- hey` -}
-- a comment that will go into a snippet

main :: IO ()
main = print "hi!"

{-
 <b name="fn_laws">1</b> <- Some html
-}