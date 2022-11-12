module Main where

import Book

main :: IO ()
main = getDataDir >>= print