{-# LANGUAGE ImportQualifiedPost #-}

module C_1_2_Writing_to_a_file(greetingTxt) where

import C_0_Setup (getDataDir)
import Relude ( Semigroup((<>)), IO, show, IOMode(WriteMode) )
import System.FilePath ((</>))
import System.IO qualified as IO
import Prelude ()

greetingTxt :: IO.FilePath
greetingTxt = "greeting.txt"

writeGreetingFile :: IO ()
writeGreetingFile = do
    dir <- getDataDir
    h <- IO.openFile (dir </> greetingTxt) WriteMode
    IO.putStrLn ("handle: " <> show h)
    IO.hPutStrLn h "hello"
    IO.hClose h
    IO.putStrLn dir

helloWorld :: IO ()
helloWorld = IO.putStrLn "hello, world!"
