{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException)
import Control.Exception.Base (catch)
import Control.Monad (when, zipWithM_)
import Converter (Config (..), hsToMd, lhsToMd, mdToLhs)
import Data.Char (toLower)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither, decodeFileThrow)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

testDir :: String
testDir = "testdata/"

main :: IO ()
main = do
  -- convert Haskell to Markdown
  let pathsHs = ((testDir ++ "/hs/") ++) <$> ["input0.hs"]
      pathsMd1 = (++ ".md") <$> pathsHs
  Config{..} <-
    decodeFileThrow (testDir ++ "/config/lima.yaml")
      `catch` (\(x :: SomeException) -> exitWith $ ExitFailure 1)
  filesHs <- mapM readFile pathsHs
  let filesMd = hsToMd (fromMaybe def configHS2MD) <$> filesHs
  zipWithM_ writeFile pathsMd1 filesMd

  -- test round-trip btw lhs and md
  let pathsLhs = ((testDir ++ "/lhs/") ++) <$> ["input0.lhs", "input1.lhs"]
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
