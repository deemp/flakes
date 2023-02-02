{-
# Top heading

- Only multi-line comments are supported for Markdown

- Haskell code snippet (use `hs`)

    ```hs
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

the following two comments will be commented out due to a config
-}

{- first special comment -}

{- second special comment -}

-- before a magic comment

{- FOURMOLU_DISABLE -}

-- after a magic comment
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

{- FOURMOLU_ENABLE -}

-- this comment will go into the Haskell snippet
module Main where -- a comment after

a1 :: Integer
a1 = 4

{-
This comment will divide Haskell snippets
-}

-- Imports

a2 :: Integer
a2 = 2

{-
## Second-level heading

some text

### Third-level heading
-}

-- Text one
-- this multiline comment should separate this line and the line after the comment
{--}
-- Text two should be ignored

{- LIMA_DISABLE -}
-- Text two to ignore
some :: String
some = "code to ignore"

{- LIMA_ENABLE -}

-- Text three

-- This comment will go into a Haskell snippet

-- | This doc will go into a Haskell snippet
class (Monoid (m a)) => Reducible m a where
  op :: m a -> Char

  -- | this too
  constr :: m a -> a -> m a

  -- | and this one
  get :: m a -> m a -> a

{-
don't put anything outside and after this comment like in `{- -} -- hey`
-}

-- a comment that will go into a snippet

{- FOURMOLU_DISABLE -}

{-| some 
doc that will be in a snippet -}
main :: IO ()
main = print "hi!"

{- FOURMOLU_ENABLE -}

{-
<b name="fn_laws">1</b> <- Some html

{- LIMA_DISABLE -}

in comments should read like {- LIMA_DISABLE -}
-}

{- LIMA_DISABLE -}

-- Some text

{- LIMA_ENABLE -}

-- shouldn't be ignored

{-
this LIMA_ENABLE reads like a comment because there's no corresponding
preceding LIMA_DISABLE
-}

{- LIMA_INDENT 4 -}

{-
1. listing
-}

-- where's lorem ipsum blah?

{- LIMA_DEDENT -}

-- final comment
