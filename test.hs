module Main where

import Converter
import Data.Char (toLower)
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))

main :: IO ()
main = do
    argsToMd <- return $ ["toMd"] ++ map ("./testing/lhs-md/"++) ["input0.lhs", "input1.lhs", "input2.lhs"]
    argsToLhs <- return $ ["toLhs"] ++ map ("./testing/md-lhs/"++) ["input0.md", "input1.md", "input2.md"]

    checkToMd <- return $ map ("./testing/lhs-md/"++) ["output0.md", "output1.md", "output2.md"]
    checkToLhs <- return $ map ("./testing/md-lhs/"++) ["output0.lhs", "output1.lhs", "output2.lhs"]

    let pathsToMd = tail argsToMd
        pathsToLhs = tail argsToLhs
        
    toMdFiles <- mapM readFile pathsToMd
    toLhsFiles <- mapM readFile pathsToLhs

    toMdOutputs <- mapM readFile checkToMd
    toLhsOutputs <- mapM readFile checkToLhs
    
    -- convert
    convertedToMd <- return $ map convertToMd toMdFiles
    convertedToLhs <- return $ map convertToLhs toLhsFiles

    -- check 
    exitWith $ if (convertedToMd == toMdOutputs && convertedToLhs == toLhsOutputs)
                then ExitFailure 1
                else ExitSuccess
