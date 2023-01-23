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

the following two comments will be ignored due to a config

```haskell
-- before a magic comment
-- after a magic comment
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
```

Some imports here

```haskell
-- this comment will go into the Haskell snippet
-- Imports
module Main where -- a comment after
```

This comment will divide Haskell snippets

```haskell
import Data.Monoid (Product (..), Sum (..))
```

## Second-level heading

some text

### Third-level heading

```haskell
-- Text one
-- Text two should be ignored
```

```haskell
-- Text three

-- This comment will go into a Haskell snippet

-- | This doc will go into a Haskell snippet
class (Monoid (m a)) => Reducible m a where
  op :: m a -> Char

  -- | this too
  constr :: m a -> a -> m a

  -- | and this one
  get :: m a -> m a -> a
```

don't put anything outside and after this comment like in `{- -} -- hey`

```haskell
-- a comment that will go into a snippet

{-| some 
doc that will be in a snippet -}
main :: IO ()
main = print "hi!"
```

<b name="fn_laws">1</b> <- Some html

{- LIMA_DISABLE -}

in comments should read like {- LIMA_DISABLE -}

```haskell
-- shouldn't be ignored
```

this LIMA_ENABLE reads like a comment because there's no corresponding
preceding LIMA_DISABLE

```haskell
-- where's lorem ipsum blah?
```
