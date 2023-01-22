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

_LIMA_DISABLE :: String
_LIMA_DISABLE = "LIMA_DISABLE"

_LIMA_ENABLE :: String
_LIMA_ENABLE = "LIMA_ENABLE"

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

-- | Convert contents of a @Haskell@ file to @Markdown@
hsToMd :: String -> String
hsToMd = unlines . reverse . (\x -> convert True False False x []) . lines
 where
  convert :: Bool -> Bool -> Bool -> [String] -> [String] -> [String]
  convert inLimaEnable inComments inSnippet (h : hs) res
    | -- disable
      -- split a snippet
      not inComments && h `startsWith` (mcOpen ++ " " ++ _LIMA_DISABLE) =
        convert False False False hs ([backticks | inSnippet] ++ res)
    | -- enable
      -- split a snippet
      not inComments && h `startsWith` (mcOpen ++ " " ++ _LIMA_ENABLE) =
        convert True False False hs res
    | -- if disabled
      not inLimaEnable =
        convert inLimaEnable False False hs res
    | -- a special comment
      -- split a snippet
      not inComments && h `startsWithAnyOf` ((mcOpen ++) <$> mcSpecial) =
        convert inLimaEnable False True hs ([h] ++ [backticksHs | not inSnippet] ++ res)
    | -- a magic comment should be ignored
      -- split a snippet
      not inComments && h `startsWithAnyOf` (((mcOpen ++ " ") ++) <$> magicComments) =
        convert inLimaEnable False False hs ([backticks | inSnippet] ++ res)
    | -- start of a multi-line comment
      not inComments && h `startsWith` mcOpen =
        let x' = drop 3 h
            pref = if inSnippet then ["", backticks] else []
            res' = if inSnippet then dropWhile (== "") res else res
         in -- if a multiline comment ends on the same line
            if h `endsWith` mcClose
              then convert inLimaEnable False False hs ([dropEnd 3 x'] ++ pref ++ res')
              else convert inLimaEnable True False hs ([x' | not (null x')] ++ pref ++ res')
    | -- end of a multiline comment
      inComments && h `startsWith` mcClose =
        convert inLimaEnable False False hs ("" : res)
    | -- copy everything from comments
      inComments =
        convert inLimaEnable True False hs (h : res)
    -- not in comments
    | -- if not in snippet, collapse consequent empty lines
      not inSnippet && null h =
        convert inLimaEnable False False hs ("" : dropWhile (== "") res)
    | -- non-empty line means the start of a Haskell snippet
      not inSnippet && not (null h) =
        convert inLimaEnable False True hs ([h, backticksHs, ""] ++ dropWhile (== "") res)
    | -- lines in snippet are copied
      otherwise =
        convert inLimaEnable False True hs (h : res)
  convert limaEnable inComments inSnippet [] res =
    [backticks | inSnippet] ++ dropWhile (== "") res