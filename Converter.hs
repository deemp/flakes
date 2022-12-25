module Converter (convertToMd, convertToLhs) where

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
convertToMd :: String -> String
convertToMd = unlines . convert "" . lines
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
convertToLhs :: String -> String
convertToLhs = unlines . convert False False "" . lines
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
        && h == "```" =
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
startsWith l r = take (length r) l == r

startsWithAnyOf :: String -> [String] -> Bool
startsWithAnyOf l = any (startsWith l)

shiftIfHeader :: String -> [String]
shiftIfHeader "" = [""]
shiftIfHeader (' ' : '#' : x) = ['#' : x]
shiftIfHeader x = [x]
