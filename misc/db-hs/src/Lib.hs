module Lib (someFunc) where

import System.Process (callCommand)

someFunc :: IO ()
someFunc = callCommand "hello"
