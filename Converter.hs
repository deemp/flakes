module Converter where

backticks :: String 
backticks = "```"

languageMark :: String
languageMark = backticks ++ "haskell"

birdticks :: String
birdticks = "<>"

-- | converts a files contents from the LHS birdtick style 
--   to markdown, replacing the code marked by birdticks with ```haskell ... ```
convertToMd :: String -> String
convertToMd = unlines . convert' "" . lines
  where
    convert' :: String -> [String] -> [String]
    convert' prev []
        | isFirstOO prev birdticks     = [backticks]     -- close code tags at the end
        | otherwise                    = []          
    convert' prev (h:t)
        | not (isFirstOO prev birdticks) &&                      -- checks for code to stars, inserts newline above code block if needed
               isFirstOO h    birdticks      = (if prev == "" then [] else [""]) ++ [languageMark, drop 2 h] ++ rest
        | isFirstOO      h    birdticks      = (drop 2 h):rest    -- checks for code
        | isFirstOO      prev birdticks      = [backticks] ++ (if h == "" then [] else [""]) ++ [h] ++ rest    -- checks for code end, insers newline after code block if needed
        | otherwise                          = h:rest
        where 
            rest = convert' h t

-- | converts a files contents from git flavoured markdown 
--   to literate haskell, replacing code marked with ```haskell ...``` with > birdticks
--   and  code marked with ``` ... ``` with < birdticks
--   quotes are converted to `NOTE:` marked lines.
convertToLhs :: String -> String
convertToLhs = unlines . convert' False False "" . lines 
  where
    convert' :: Bool -> Bool -> String -> [String] -> [String]
    convert' inCode inSample prev [] = []
    convert' inCode inSample prev (h:t)
        | inCode   && h /= backticks   = ["> " ++ h] ++ (convert' True False h t)       -- handles code
        | inSample && h /= backticks   = ["< " ++ h] ++ (convert' False True h t)       -- handles code sample
        | headingN h /= 0              = [headingO h ++ (tail $ dropWhile (=='#') h) ++ headingC h] ++ rest   -- converts headings to html
        | h == languageMark            = (if prev == "" then [] else [""]) ++ convert' True False prev t      -- handles code block starts, adds newline if needed
        | (inCode || inSample) 
            && h == "```"              = (if length t > 0 && (head t) == "" 
                                            then [] 
                                            else [""]) 
                                          ++ convert' False False prev t  -- handles code and sample block ends
        | take 3 h == backticks        = (if prev == "" then [] else [""]) ++ convert' False True prev t      --  handles sample block starts, adds newline if needed
        | isFirstOO h ">"              = ["NOTE: " ++ drop 2 h] ++ rest                                       -- handles quotes
        | otherwise                    = h:rest
        where 
            headingO h = "<h" ++ show (headingN h) ++ ">"    -- heading open tag
            headingC h = "</h" ++ show (headingN h) ++ ">"   -- heading close tag
            headingN h = length $ takeWhile (=='#') h        -- looks for headings
            rest = convert' False False h t               

-- | uses @isFirst@ to check multiple characters
isFirstOO :: String -> [Char] -> Bool
isFirstOO l e = or $ map (isFirst l) e

-- | checks wether the first two characters of a string are 
--   a given character and ' '
--   useful to look for code and quotes
isFirst :: String -> Char -> Bool
isFirst []        _ = False
isFirst (h:' ':_) a = a == h
isFirst (h:_)     a = False

