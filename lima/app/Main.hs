module Main (main) where

import Control.Monad (when)
import Converter (hsToMd, lhsToMd, mdToLhs)
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

usage :: String
usage =
    "Usage: \
    \lima (md2lhs|lhs2md|hs2md) file1 [file2] [...]"

exitErr :: String -> IO a
exitErr msg = putStrLn (msg ++ "\n" ++ usage) >> exitWith (ExitFailure 1)

main :: IO ()
main = do
    args <- getArgs

    when (length args < 2) $
        exitErr "not enough arguments given."

    let paths = tail args

    files <- mapM readFile paths

    let funcExt = case map toLower $ head args of
            "md2lhs" -> Just (mdToLhs, "lhs")
            "lhs2md" -> Just (lhsToMd, "md")
            "hs2md" -> Just (hsToMd, "md")
            _ -> Nothing

    when (isNothing funcExt) $
        exitErr $
            "command " ++ head args ++ " not recongised"

    let (func, ext) = fromJust funcExt
    -- convert
    let converted = func <$> files
    -- write back
    mapM_ (uncurry writeFile) $ zip (map (++ "." ++ ext) paths) converted
    -- print finished
    putStrLn "finished conversions"
