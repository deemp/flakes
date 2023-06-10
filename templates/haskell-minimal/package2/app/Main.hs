module Main (main) where

import Lib2 (someFunc)

main :: IO ()
main = someFunc

{- This code will be evaluated by HLS
>>> 2 + 2
4
-}
