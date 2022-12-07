module Main(main) where

import Control.Lens ((^?))
import Data.Aeson (Value (..), object)
import Data.Aeson.Lens (key)
import Data.Maybe (fromMaybe)

val :: Value
val = object [("executables", object [("Book", object [("deps", "vals")])])]

-- f :: Value
ff :: Value
ff = fromMaybe (object []) (val ^? key "executables" . key "Book")

-- _Object


-- s = val ^? key "a"

{-
>>> f
Object (fromList [])
-}

main :: IO ()
main = putStrLn "Hello!"