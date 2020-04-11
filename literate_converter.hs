module Main where

import Converter
import Data.Char (toLower)
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))

useage :: String
useage = "Useage: \
         \lhsc (toLhs|toMd) file1 [file2] [...]"

exitErr :: String -> IO a
exitErr msg = putStrLn (msg ++ "\n" ++ useage) >> exitWith (ExitFailure 1)

main = do
    args <- getArgs

    when (length args < 2) $
        exitErr "not enough arguments given."

    let paths = tail args

    files <- mapM (readFile) paths

    let (func, ex) = case (map toLower $ head args) of
                "tolhs" -> (convertToLhs, "lhs")
                "tomd"  -> (convertToMd, "md")
                _       -> (id, "id") 
    
    when (ex == "id") $
        exitErr $ "command " ++ head args ++ " not recongised"

    -- convert
    converted <- return $ map func files
    -- write back
    mapM_ (uncurry writeFile) $ zip (map (++ "." ++ ex) paths) converted
    -- print finished
    putStrLn "finished conversions"
