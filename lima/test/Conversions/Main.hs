{-# OPTIONS_GHC -fplugin Debug.Breakpoint #-}

import Converter (Config, Format (..), Token (..), Tokens, User, def, exampleNonTexTokens, exampleTexTokens, normalizeTokens, selectFromTokens, selectToTokens, showFormatExtension, showFormatName, stripEmpties, texHaskellCodeEnd, texHaskellCodeStart, toInternalConfig)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
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

-- TODO test initial haskell code indentation is the same as after parsing
-- so, generate lines with indentation relative to the previous indent token

-- TODO test no newline as the first line in fromTokens

-- TODO no newlines in the generated lines

--

selectDialectName :: Format -> String
selectDialectName = \case
  TeX -> showFormatName TeX
  _ -> "Non-" <> showFormatName TeX

testDialectWrite :: FilePath -> Tokens -> Format -> TestTree
testDialectWrite dir tokens format =
  testCase [i|Write #{showFormatName format} code|] do
    let text = selectFromTokens def format tokens
    T.writeFile [i|#{dir}/test.#{showFormatExtension format}|] text
    assertEqual "Roundtrip" tokens (selectToTokens def format text)

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

testFormatsWrite :: FilePath -> [Format] -> Tokens -> [TestTree]
testFormatsWrite dir formats tokens =
  (testDialectWrite dir tokens <$> formats) <> [writeTokens dir tokens]

selectTokens :: Format -> Tokens
selectTokens = \case
  TeX -> exampleTexTokens
  _ -> exampleNonTexTokens

selectFormats :: Format -> [Format]
selectFormats = \case
  TeX -> [Hs, Md, Lhs, TeX]
  _ -> [Hs, Md, Lhs]

testWrite :: Format -> TestTree
testWrite format =
  let dir = [i|#{testDir}/#{showFormatExtension format}|]
   in withResource (createDirectoryIfMissing True dir) pure $
        const $
          testGroup [i|Using #{selectDialectName format} tokens|] $
            (testFormatsWrite dir (selectFormats format) (selectTokens format))

alphabet :: MonadGen m => m Char
alphabet = Gen.alphaNum

genNonEmpty :: Gen (NonEmpty T.Text)
genNonEmpty = Gen.nonEmpty (Range.constant 1 5) genLine

genLine :: Gen T.Text
genLine = do
  indent <- Gen.text (Range.constant 0 5) (pure ' ')
  contents <- Gen.text (Range.constant 1 5) alphabet
  pure $ indent <> contents

genLines :: Gen [T.Text]
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
  someLines <- genNonEmpty
  pure $ Comment{someLines}

genText :: Gen Token
genText = do
  someLines <- genNonEmpty
  pure Text{..}

genNonDisabled :: (?genCode :: GenCode) => Gen Tokens
genNonDisabled =
  Gen.choice $
    [?genCode] <> (((: []) <$>) <$> [genComment, genText, genIndent, genDedent])

genDisabled :: Env => Gen Token
genDisabled = do
  tokens <- genNonDisabled
  let manyLines = reverse $ T.lines $ selectFromTokens ?config ?format tokens
  pure $ Disabled{manyLines}

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
  manyLines <- genLines
  let manyLines' = if null (stripEmpties manyLines) then ["a"] else manyLines
  pure $
    [ Text{someLines = config ^. texHaskellCodeStart :| []}
    , HaskellCode{manyLines = manyLines'}
    , Text{someLines = config ^. texHaskellCodeEnd :| []}
    ]

-- Code tokens from tex files can only be recognized in a specific sequence
-- That's why, they're generated in a list
notTexGenCode :: Gen [Token]
notTexGenCode = do
  manyLines <- genLines
  let manyLines' = if null (stripEmpties manyLines) then ["a"] else manyLines
  pure [HaskellCode{manyLines = manyLines'}]

-- Tripping
trippingTokens :: (MonadTest m, ?config :: Config User, ?format :: Format) => Tokens -> m ()
trippingTokens tokens =
  tripping tokens (selectFromTokens ?config ?format) (Just . selectToTokens ?config ?format)

testTrippingTokens :: Env => Property
testTrippingTokens = property do
  tokens <- Gen.sample $ genTokens
  -- breakpointM
  trippingTokens tokens

type GenCode = Gen [Token]

type Env = (?config :: Config User, ?format :: Format, ?genCode :: GenCode)

selectGenCode :: Format -> ((?config :: Config User) => GenCode)
selectGenCode = \case
  TeX -> texGenCode
  _ -> notTexGenCode

testTripping :: Format -> TestTree
testTripping format =
  let ?config = def; ?format = format
   in let ?genCode = selectGenCode format
       in testProperty [i|Roundtrip #{selectDialectName format} tokens for all formats|] $ testTrippingTokens