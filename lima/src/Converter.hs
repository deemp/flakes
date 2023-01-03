module Converter (lhsToMd, mdToLhs, hsToMd) where

import Data.List (isPrefixOf, isSuffixOf)

backticks :: String
backticks = "```"

haskellSnippet :: String
haskellSnippet = backticks ++ "haskell"

consoleSnippet :: String
consoleSnippet = backticks ++ "console"

chooseSnippetType :: String -> String
chooseSnippetType s
  | s `startsWith` birdTrack = haskellSnippet
  | otherwise = consoleSnippet

birdTrack :: String
birdTrack = "> "

reverseBirdTrack :: String
reverseBirdTrack = "< "

birdTracks :: [String]
birdTracks = [birdTrack, reverseBirdTrack]

-- | convert a file's contents from the LHS birdtick style
-- to markdown, replacing the code marked by birdticks with ```haskell ... ```
lhsToMd :: String -> String
lhsToMd = unlines . convert "" . lines
 where
  convert :: String -> [String] -> [String]
  convert prev []
    | -- close code tags at the end
      prev `startsWithAnyOf` birdTracks =
        [backticks]
    | otherwise = []
  convert prev (h : t)
    | -- check for Haskell code to start
      -- insert newline above code block if needed
      not (prev `startsWithAnyOf` birdTracks)
        && (h `startsWithAnyOf` birdTracks) =
        (["" | prev /= ""]) ++ [chooseSnippetType h, drop 2 h] ++ rest
    | -- check for code
      h `startsWithAnyOf` birdTracks =
        drop 2 h : rest
    | -- check for code end, insert newline after code block if needed
      prev `startsWithAnyOf` birdTracks =
        [backticks] ++ (["" | h /= ""]) ++ [h] ++ rest
    | h `startsWith` (' ' : birdTrack) =
        drop 1 h : rest
    | otherwise = shiftIfHeader h ++ rest
   where
    rest = convert h t

-- | convert a file's contents from git flavoured markdown
-- to literate haskell, replacing code marked with ```haskell ...``` with > birdticks
-- and  code marked with ``` ... ``` with < birdticks
-- quotes are converted to `NOTE:` marked lines.
mdToLhs :: String -> String
mdToLhs = unlines . convert False False "" . lines
 where
  convert :: Bool -> Bool -> String -> [String] -> [String]
  convert inHsCode inSample prev [] = []
  convert inHsCode inSample prev (h : t)
    | -- handle code block starts, add newline if needed
      h == haskellSnippet =
        (["" | prev /= ""]) ++ convert True False prev t
    | -- handle code
      inHsCode && h /= backticks =
        ("> " ++ h) : convert True False h t
    | -- handle code sample
      inSample && h /= backticks =
        ("< " ++ h) : convert False True h t
    | -- move headings one space to right
      isHeading h =
        (' ' : h) : rest
    | -- handle code and sample block ends
      (inHsCode || inSample)
        && h == backticks =
        (["" | null t || head t /= ""])
          ++ convert False False prev t
    | -- handle sample block starts, add newline if needed
      take 3 h == backticks =
        (["" | prev /= ""]) ++ convert False True prev t
    | -- handle quotes
      h `startsWith` birdTrack =
        (" >" ++ drop 1 h) : rest
    | otherwise = h : rest
   where
    -- count headings
    isHeading h = not (null (takeWhile (== '#') h))
    rest = convert False False h t

startsWith :: String -> String -> Bool
startsWith = flip isPrefixOf

startsWithAnyOf :: String -> [String] -> Bool
startsWithAnyOf l = any (startsWith l)

shiftIfHeader :: String -> [String]
shiftIfHeader "" = [""]
shiftIfHeader (' ' : '#' : x) = ['#' : x]
shiftIfHeader x = [x]

magicComments :: [String]
magicComments = ["FOURMOLU_DISABLE", "FOURMOLU_ENABLE"]

endsWith :: String -> String -> Bool
endsWith = flip isSuffixOf

-- multi-line comments
mcOpen :: String
mcOpen = "{-"

mcClose :: String
mcClose = "-}"

mcSpecial :: [String]
mcSpecial = ["@", "#"]

dropEnd :: Int -> String -> String
dropEnd n s
  | n > 0 && not (null s) = dropEnd (n - 1) (init s)
  | otherwise = s

backticksHs :: [Char]
backticksHs = backticks ++ "haskell"

-- will prepend lines to an answer
hsToMd :: String -> String
hsToMd = unlines . reverse . (\x -> convert False False "" x []) . lines
 where
  convert :: Bool -> Bool -> String -> [String] -> [String] -> [String]
  convert inComments inSnippet prev (h : t) res
    | -- a special comment
      h `startsWithAnyOf` ((mcOpen ++) <$> mcSpecial) =
        convert False True h t ([h] ++ [backticksHs | not inSnippet] ++ res)
    | -- a magical comment
      h `startsWithAnyOf` (((mcOpen ++ " ") ++) <$> magicComments) =
        convert False False h t ([backticks | inSnippet] ++ res)
    | -- start of a multi-line comment
      h `startsWith` mcOpen =
        let x' = drop 3 h
            pref = if inSnippet then ["", backticks] else []
            res' = if inSnippet then dropWhile (== "") res else res
         in if h `endsWith` mcClose -- multiline comment ends on the same line
              then convert False False h t ([dropEnd 3 x'] ++ pref ++ res')
              else convert True False h t ([x' | not (null x')] ++ pref ++ res')
    | -- end of a multiline comment
      h `startsWith` mcClose =
        convert False False "" t ("" : res)
    | -- copy everything from comments
      inComments =
        convert True False h t (h : res)
    -- not in comments
    | -- if not in snippet, collapse consequent empty lines
      not inSnippet && null h =
        convert False False h t (["" | prev /= ""] ++ res)
    | -- not empty line means the start of a Haskell snippet
      not inSnippet && not (null h) =
        convert False True h t ([h, backticksHs] ++ ["" | prev /= ""] ++ res)
    | -- lines in snippet are copied
      otherwise =
        convert False True h t (h : res)
  convert inComments inSnippet prev [] res =
    if inSnippet
      then backticks : res
      else dropWhile (== "") res