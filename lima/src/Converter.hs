{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

-- | Functions to convert @Haskell@ to @Markdown@ and between @Literate Haskell@ (@.lhs@) and @Markdown@.
module Converter (hsToMd, mdToHs, lhsToMd, mdToLhs, Config (..), ConfigHsMd (..)) where

import Data.Default (Default)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Yaml (FromJSON (..))
import Data.Yaml.Aeson (withObject, (.:), (.:?))
import GHC.Generics (Generic)

-- | App config.
newtype Config = Config {configHsMd :: Maybe ConfigHsMd} deriving (Generic, Default)

instance FromJSON Config where
  parseJSON = withObject "Configs" (\v -> Config <$> v .:? "hs-md")

-- | Config for @Haskell@ to @Markdown@ converter.
newtype ConfigHsMd = ConfigHs2Md {specialComments :: [String]} deriving (Generic, Default)

instance FromJSON ConfigHsMd where
  parseJSON =
    withObject
      "CommentsToIgnore"
      (\v -> ConfigHs2Md <$> v .: "special-comments")

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

-- | Convert @Literate Haskell@ to @Markdown@.
--
-- Convert @LHS@ birdtick style to @Markdown@, replacing the code marked by birdticks with @```haskell ... ```@.
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

-- | Convert @Markdown@ file to @Literate Haskell@.
--
-- Replace code marked with @```haskell ...```@ with birdticks (@>@)
-- and code marked with @``` ... ```@ with reverse birdticks (@<@).
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

-- | Comments that should be ignored for some reason
--
-- FOURMOLU_DISABLE is ignored because it's a special comment and shouldn't be visible in a `.md`
specialCommentsDefault :: [String]
specialCommentsDefault = ["FOURMOLU_DISABLE", "FOURMOLU_ENABLE"]

endsWith :: String -> String -> Bool
endsWith = flip isSuffixOf

-- multi-line comments
mcOpen :: String
mcOpen = "{-"

mcOpenSpace :: String
mcOpenSpace = mcOpen ++ " "

mcClose :: String
mcClose = "-}"

mcCloseSpace :: String
mcCloseSpace = " -}"

dropEnd :: Int -> String -> String
dropEnd n s = reverse (drop n (reverse s))

backticksHs :: String
backticksHs = backticks ++ "haskell"

dropEmpties :: [String] -> [String]
dropEmpties = dropWhile (== "")

-- Markdown comments

-- | Open a @Markdown@ comment
mdcOpen :: String
mdcOpen = "<!--"

-- | Close a @Markdown@ comment
mdcClose :: String
mdcClose = "-->"

mdcOpenSpace :: String
mdcOpenSpace = mdcOpen ++ " "

mdcCloseSpace :: String
mdcCloseSpace = " " ++ mdcClose

stripMc :: String -> String
stripMc x = dropEnd (length mcCloseSpace) (drop (length mcOpenSpace) x)

-- | Convert @Haskell@ to @Markdown@.
--
-- Multi-line comments are copied as text blocks and @Haskell@ code is copied as @Haskell@ snippets.
hsToMd :: ConfigHsMd -> String -> String
hsToMd ConfigHs2Md{..} = unlines . dropWhile (== "") . reverse . (\x -> convert True False False x []) . lines
 where
  specialComments_ = specialCommentsDefault ++ specialComments
  convert :: Bool -> Bool -> Bool -> [String] -> [String] -> [String]
  convert inLimaEnable inComments inSnippet (h : hs) res
    | not inComments =
        if
            | -- disable
              -- split a snippet
              h `startsWith` (mcOpenSpace ++ _LIMA_DISABLE) ->
                (convert False False False hs)
                  ([mdcOpenSpace ++ _LIMA_DISABLE] ++ [backticks | inSnippet] ++ res)
            | -- enable
              h `startsWith` (mcOpenSpace ++ _LIMA_ENABLE) ->
                convert True False inSnippet hs ((_LIMA_ENABLE ++ mdcCloseSpace) : res)
            | -- if disabled
              not inLimaEnable ->
                convert inLimaEnable False False hs (h : res)
            | -- a special comment
              -- comment should be in multi-line style like '{- FOURMOLU_DISABLE -}'
              -- splits a snippet
              h `startsWithAnyOf` ((mcOpenSpace ++) <$> specialComments_) ->
                (convert inLimaEnable False False hs)
                  ([mdcOpenSpace ++ stripMc h ++ mdcCloseSpace] ++ ["" | inSnippet] ++ [backticks | inSnippet] ++ dropEmpties res)
            | -- start of a multi-line comment
              -- it should be either like '{- ...' or '{-\n'
              (h `startsWith` mcOpenSpace || h == mcOpen) ->
                let x' = drop 3 h
                    pref = "" : [backticks | inSnippet]
                    res' = if inSnippet then dropEmpties res else res
                 in -- if a multiline comment ends on the same line
                    -- it should end with '-}'
                    if h `endsWith` mcClose
                      then convert inLimaEnable False False hs ([dropEnd (length mcCloseSpace) x'] ++ pref ++ res')
                      else convert inLimaEnable True False hs ([x' | not (null x')] ++ pref ++ res')
            | -- non-empty line means the start of a Haskell snippet
              not inSnippet ->
                if not (null h)
                  then -- non-empty line means the start of a Haskell snippet
                    convert inLimaEnable False True hs ([h, backticksHs] ++ squashEmpties res)
                  else -- if not in snippet, collapse consequent empty lines
                    convert inLimaEnable False False hs res
            | inSnippet ->
                convert inLimaEnable False True hs (h : res)
    | inComments =
        if
            | -- end of a multiline comment
              h `startsWith` mcClose ->
                convert inLimaEnable False False hs res
            | -- copy everything from comments
              otherwise ->
                convert inLimaEnable True False hs (h : res)
  convert _ _ inSnippet [] res =
    [backticks | inSnippet] ++ dropEmpties res

stripMdc :: String -> String
stripMdc x = dropEnd (length mdcCloseSpace) (drop (length mdcOpenSpace) x)

squashEmpties :: [String] -> [String]
squashEmpties = ([""] ++) . dropEmpties

-- | Convert @Markdown@ to @Haskell@.
--
-- Multi-line comments are copied as text blocks and @Haskell@ code is copied as @Haskell@ snippets.
mdToHs :: ConfigHsMd -> String -> String
mdToHs ConfigHs2Md{..} = unlines . dropWhile (== "") . reverse . (\x -> convert False False False x []) . lines
 where
  specialComments_ = specialCommentsDefault ++ specialComments
  closeTextIf x = [mcClose | x]
  convert :: Bool -> Bool -> Bool -> [String] -> [String] -> [String]
  convert inText inSnippet inComments (h : hs) res
    | inComments =
        if
            | -- enable
              h `startsWith` (_LIMA_ENABLE ++ mdcCloseSpace) ->
                -- split text
                (convert inText inSnippet False hs)
                  ([mcOpenSpace ++ _LIMA_ENABLE ++ mcCloseSpace] ++ ["" | inText] ++ closeTextIf inText ++ res)
            | -- in a comment
              otherwise ->
                convert False False True hs (h : res)
    | not inSnippet =
        if
            | -- found comments
              h `startsWith` (mdcOpenSpace ++ _LIMA_DISABLE) ->
                (convert False False True hs)
                  ([mcOpenSpace ++ _LIMA_DISABLE ++ mcCloseSpace] ++ ["" | inText] ++ closeTextIf inText ++ res)
            | -- found a special comment
              h `startsWithAnyOf` ((mdcOpenSpace ++) <$> specialComments_) ->
                (convert False False False hs)
                  ([mcOpenSpace ++ stripMdc h ++ mcCloseSpace] ++ ["" | inText] ++ closeTextIf inText ++ res)
            | -- start of a haskell snippet
              h `startsWith` backticksHs ->
                convert False True False hs (closeTextIf inText ++ squashEmpties res)
            | not inText ->
                if
                    | null h -> convert inText False False hs res
                    | otherwise -> convert True False False hs ([h, mcOpen, ""] ++ res)
            | -- copy text line by line
              otherwise ->
                convert True False False hs (h : res)
    | otherwise =
        if h == backticks
          then -- end of a snippet
            convert False False False hs res
          else -- in a snippet
            convert False True False hs (h : res)
  convert inText _ _ [] res = [mcClose | inText] ++ res
