{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Filesystem.Path.CurrentOS as Path ()
import Options.Applicative (Parser, action, argument, execParser, helper, info, metavar, str)

main :: IO ()
main = execParser (info (helper <*> p) mempty) >>= print

p :: Parser String
p = argument str (metavar "PATH" <> action "directory")

s :: IO ()
s = putStrLn "Hello!"

-- $> s

