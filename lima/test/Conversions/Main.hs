{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

module Main (main) where

import Converter (Config, Dialect (..), Token (..), Tokens, User, def, exampleNonTexTokens, exampleTexTokens, normalizeTokens, selectFromTokens, selectIntoTokens, showDialectExtension, showDialectName, stripEmpties, texHaskellCodeEnd, texHaskellCodeStart, toInternalConfig)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import Hedgehog (Gen, MonadGen, MonadTest, Property, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing)
import System.IO (IOMode (WriteMode), withFile)
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Text.Pretty.Simple (CheckColorTty (..), OutputOptions (..), StringOutputStyle (Literal), defaultOutputOptionsDarkBg, pHPrintOpt)

testDir :: String
testDir = "testdata"

main :: IO ()
main =
  defaultMain $
    testGroup
      "Top"
      [ testWrite Md
      , testWrite TeX
      , testTripping TeX
      , testTripping Md
      ]

selectDialectName :: Dialect -> String
selectDialectName = \case
  TeX -> showDialectName TeX
  _ -> "Non-" <> showDialectName TeX

testDialectWrite :: FilePath -> Tokens -> Dialect -> TestTree
testDialectWrite dir tokens dialect =
  testCase [i|Write #{showDialectName dialect} code|] do
    let text = selectFromTokens def dialect tokens
    writeFile [i|#{dir}/test.#{showDialectExtension dialect}|] text
    assertEqual "Roundtrip" tokens (selectIntoTokens def dialect text)

writeTokens :: FilePath -> Tokens -> TestTree
writeTokens dir tokens = testCase "Write tokens to a file" $
  withFile [i|#{dir}/tokens.hs|] WriteMode $
    \h ->
      pHPrintOpt
        CheckColorTty
        ( defaultOutputOptionsDarkBg
            { outputOptionsStringStyle = Literal
            }
        )
        h
        tokens

testDialectsWrite :: FilePath -> [Dialect] -> Tokens -> [TestTree]
testDialectsWrite dir dialects tokens =
  (testDialectWrite dir tokens <$> dialects) <> [writeTokens dir tokens]

selectTokens :: Dialect -> Tokens
selectTokens = \case
  TeX -> exampleTexTokens
  _ -> exampleNonTexTokens

selectDialects :: Dialect -> [Dialect]
selectDialects = \case
  TeX -> [Hs, Md, Lhs, TeX]
  _ -> [Hs, Md, Lhs]

testWrite :: Dialect -> TestTree
testWrite dialect =
  let dir = [i|#{testDir}/#{showDialectExtension dialect}|]
   in withResource (createDirectoryIfMissing True dir) pure $
        const $
          testGroup [i|Using #{selectDialectName dialect} tokens|] $
            (testDialectsWrite dir (selectDialects dialect) (selectTokens dialect))

alphabet :: MonadGen m => m Char
alphabet = Gen.alphaNum

genNonEmpty :: Gen (NonEmpty String)
genNonEmpty = Gen.nonEmpty (Range.constant 1 5) genLine

genLine :: Gen String
genLine = do
  indent <- Gen.string (Range.constant 0 5) (pure ' ')
  contents <- Gen.string (Range.constant 1 5) alphabet
  pure $ indent <> contents

genLines :: Gen [String]
genLines = Gen.list (Range.constant 1 5) genLine

-- Tokens

genIndent :: Gen Token
genIndent = do
  n <- Gen.int (Range.constant 0 10)
  pure Indent{..}

genDedent :: Gen Token
genDedent = pure Dedent

genComment :: Gen Token
genComment = do
  body <- genNonEmpty
  pure $ Comment{body}

genText :: Gen Token
genText = do
  someLines <- stripEmpties <$> genLines
  pure Text{..}

genNonDisabled :: (?genCode :: GenCode) => Gen Tokens
genNonDisabled =
  Gen.choice $
    [?genCode] <> (((: []) <$>) <$> [genComment, genText, genIndent, genDedent])

genDisabled :: Env => Gen Token
genDisabled = do
  tokens <- genNonDisabled
  let someLines = reverse $ lines $ selectFromTokens ?config ?dialect tokens
  pure $ Disabled{someLines}

genTokensSublist :: Env => Gen Tokens
genTokensSublist =
  Gen.choice $ [genNonDisabled, (: []) <$> genDisabled]

genTokens :: Env => Gen Tokens
genTokens = do
  subLists <- Gen.list (Range.singleton 1000) genTokensSublist
  pure $ normalizeTokens $ concat subLists

-- Code tokens from tex files can only be recognized in a specific sequence
-- That's why, they're generated in a list
texGenCode :: (?config :: Config User) => Gen [Token]
texGenCode = do
  let config = toInternalConfig ?config
  someLines <- genLines
  let someLines' = if null (stripEmpties someLines) then ["a"] else someLines
  pure $
    [ Text{someLines = [config ^. texHaskellCodeStart]}
    , HaskellCode{someLines = someLines'}
    , Text{someLines = [config ^. texHaskellCodeEnd]}
    ]

-- Code tokens from tex files can only be recognized in a specific sequence
-- That's why, they're generated in a list
notTexGenCode :: Gen [Token]
notTexGenCode = do
  someLines <- genLines
  let someLines' = if null (stripEmpties someLines) then ["a"] else someLines
  pure [HaskellCode{someLines = someLines'}]

-- Tripping
trippingTokens :: (MonadTest m, ?config :: Config User, ?dialect :: Dialect) => Tokens -> m ()
trippingTokens tokens =
  tripping tokens (selectFromTokens ?config ?dialect) (Just . selectIntoTokens ?config ?dialect)

testTrippingTokens :: Env => Property
testTrippingTokens = property do
  tokens <- Gen.sample $ genTokens
  -- breakpointM
  trippingTokens tokens

type GenCode = Gen [Token]

type Env = (?config :: Config User, ?dialect :: Dialect, ?genCode :: GenCode)

selectGenCode :: Dialect -> ((?config :: Config User) => GenCode)
selectGenCode = \case
  TeX -> texGenCode
  _ -> notTexGenCode

testTripping :: Dialect -> TestTree
testTripping dialect =
  let ?config = def; ?dialect = dialect
   in let ?genCode = selectGenCode dialect
       in testProperty [i|Roundtrip #{selectDialectName dialect} tokens for all dialects|] $ testTrippingTokens