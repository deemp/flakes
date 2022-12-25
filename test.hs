module Main (main) where

import Control.Monad (when, zipWithM_)
import Converter (convertToLhs, convertToMd)
import Data.Char (toLower)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

testDir :: String
testDir = "testdata/"

main :: IO ()
main = do
    let pathsLhs = (testDir ++) <$> ["input0.lhs", "input1.lhs"]
        pathsMd = (++ ".md") <$> pathsLhs
        pathsLhs' = (++ ".lhs") <$> pathsMd
    filesLhs <- mapM readFile pathsLhs
    let filesMd = convertToMd <$> filesLhs
        filesLhs' = convertToLhs <$> filesMd
    zipWithM_ writeFile pathsMd filesMd
    zipWithM_ writeFile pathsLhs' filesLhs'
    exitWith $
        if filesLhs == filesLhs'
            then ExitSuccess
            else ExitFailure 1
