module Main(main) where

import Converter ( convertToMd, convertToLhs )
import Data.Char (toLower)
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))
import Data.Maybe (isNothing, fromJust)

usage :: String
usage = "Usage: \
         \lima (toLhs|toMd) file1 [file2] [...]"

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
                "tolhs" -> Just (convertToLhs, "lhs")
                "tomd"  -> Just (convertToMd, "md")
                _       -> Nothing

    when (isNothing funcExt) $
        exitErr $ "command " ++ head args ++ " not recongised"
    
    let (func, ext) = fromJust funcExt
    -- convert
    let converted = func <$> files
    -- write back
    mapM_ (uncurry writeFile) $ zip (map (++ "." ++ ext) paths) converted
    -- print finished
    putStrLn "finished conversions"
