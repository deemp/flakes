{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException)
import Control.Exception.Base (catch)
import Control.Monad (when, zipWithM_)
import Converter (Config (..), hsToMd, lhsToMd, mdToHs, mdToLhs)
import Data.Char (toLower)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Yaml (decodeFileEither, decodeFileThrow)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

testDir :: String
testDir = "testdata"

main :: IO ()
main = do
  -- test round-trip btw hs and md
  let pathsHs1 = (\x -> [i|#{testDir}/hs/input#{x}.hs|]) <$> [0 :: Int]
      pathsMd1 = (++ ".md") <$> pathsHs1
      pathsHs2 = (++ ".hs") <$> pathsMd1
  Config{..} <-
    decodeFileThrow [i|#{testDir}/config/lima.yaml|]
      `catch` (\(x :: SomeException) -> exitWith $ ExitFailure 1)
  contentsHs1 <- mapM readFile pathsHs1
  let contentsMd1 = hsToMd (fromMaybe def configHsMd) <$> contentsHs1
      contentsHs2 = mdToHs (fromMaybe def configHsMd) <$> contentsMd1
  zipWithM_ writeFile pathsMd1 contentsMd1
  zipWithM_ writeFile pathsHs2 contentsHs2

  -- test round-trip btw lhs and md
  let pathsLhs1 = (\x -> [i|#{testDir}/lhs/input#{x}.lhs|]) <$> [0 :: Int, 1]
      pathsMd1 = (++ ".md") <$> pathsLhs1
      pathsLhs2 = (++ ".lhs") <$> pathsMd1
  contentsLhs1 <- mapM readFile pathsLhs1
  let contentsMd1 = lhsToMd <$> contentsLhs1
      contentsLhs2 = mdToLhs <$> contentsMd1
  zipWithM_ writeFile pathsMd1 contentsMd1
  zipWithM_ writeFile pathsLhs2 contentsLhs2

  exitWith $
    if contentsLhs1 == contentsLhs2 && contentsHs1 == contentsHs2
      then ExitSuccess
      else ExitFailure 1
