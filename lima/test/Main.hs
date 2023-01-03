module Main (main) where

import Control.Monad (when, zipWithM_)
import Converter (hsToMd, lhsToMd, mdToLhs)
import Data.Char (toLower)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

testDir :: String
testDir = "testdata/"

main :: IO ()
main = do
    -- write a Haskell file
    let pathsHs = (testDir ++) <$> ["input2.hs"]
        pathsMd1 = (++ ".md") <$> pathsHs
    filesHs <- mapM readFile pathsHs
    let filesMd = hsToMd <$> filesHs
    zipWithM_ writeFile pathsMd1 filesMd
    
    -- test round-trip btw lhs and md
    let pathsLhs = (testDir ++) <$> ["input0.lhs", "input1.lhs"]
        pathsMd = (++ ".md") <$> pathsLhs
        pathsLhs' = (++ ".lhs") <$> pathsMd
    filesLhs <- mapM readFile pathsLhs
    let filesMd = lhsToMd <$> filesLhs
        filesLhs' = mdToLhs <$> filesMd
    zipWithM_ writeFile pathsMd filesMd
    zipWithM_ writeFile pathsLhs' filesLhs'

    exitWith $
        if filesLhs == filesLhs'
            then ExitSuccess
            else ExitFailure 1
