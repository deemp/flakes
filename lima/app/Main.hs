{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException (SomeException), bracketOnError, catch, throwIO)
import Control.Exception.Base (try)
import Control.Monad (when, zipWithM_, (>=>))
import Converter (Config (..), ConfigHs2Md (..), hsToMd, lhsToMd, mdToLhs)
import Data.Aeson.Types (prependFailure)
import Data.Char (toLower)
import Data.Default (def)
import Data.Either (isLeft)
import Data.Foldable (Foldable (..))
import Data.List (intersperse)
import Data.Maybe (fromJust, isNothing)
import Data.String (IsString)
import Data.Traversable (forM)
import Data.Yaml (FromJSON (..), ParseException, Value (..), withObject, (.:), (.:?))
import Data.Yaml.Aeson (decodeFileEither, withArray)
import Data.Yaml.Parser (typeMismatch)
import GHC.Generics (Generic)
import Options.Applicative
import Options.Applicative.Help (Doc, bold, colon, comma, dot, fill, indent, lparen, nest, rparen, softline, text, (<+>))
import Options.Applicative.Help.Pretty (hardline)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

data CommandType = HS2MD | LHS2MD | MD2LHS deriving (Show)

data Options = Options
  { commandType :: CommandType
  , config :: Maybe FilePath
  , files :: [FilePath]
  }
  deriving (Show)

-- TODO make config and --file arguments mutually exclusive

parseConfig :: Parser ([FilePath], Maybe FilePath)
parseConfig = do
  file <- some $ strOption (long "file" <> short 'f' <> metavar "FILE" <> help "Path to a file to convert")
  config <- optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Path to a config"))
  return (file, config)

mkConfigParser :: String -> CommandType -> Doc -> Mod CommandFields Options
mkConfigParser command_ commandType progDesc_ =
  command
    command_
    ( info
        ((\(files, config) -> Options{..}) <$> parseConfig)
        (progDescDoc $ Just progDesc_)
    )

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

lima :: Parser Options
lima =
  subparser $
    foldMap
      (uncurry3 mkConfigParser)
      [ ("hs2md", HS2MD, "Convert" <-> bold "Haskell" <-> "to" <-> bold "Markdown")
      , ("md2lhs", MD2LHS, "Convert" <-> bold "Markdown" <-> "to" <-> bold "Literate Haskell")
      , ("lhs2md", LHS2MD, "Convert" <-> bold "Literate Haskell" <-> "to" <-> bold "Markdown")
      ]

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' x f g = maybe f g x

either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' x f g = either f g x

descriptionBlock :: [Doc] -> Doc
descriptionBlock desc = fold (intersperse softline desc) <> hardline

(<->) :: Doc -> Doc -> Doc
x <-> y = x <> softline <> y

fill' :: Doc
fill' = fill 100 $ text ""

header_ :: Doc
header_ =
  descriptionBlock
    [ bold "lima" <-> "converts" <-> bold "Haskell" <> lparen <> bold ".hs" <> rparen
    , "to" <-> bold "Markdown" <> lparen <> bold ".md" <> rparen
    , "and between" <-> bold "Literate Haskell" <> lparen <> bold ".lhs" <> rparen
    , "and" <-> bold "Markdown" <> lparen <> bold ".md" <> rparen <> dot
    , fill'
    , "Learn more about a command by running:" <-> bold "lima COMMAND"
    , fill'
    , "Example usage:" <> fill'
    , fill'
    , bold "lima hs2md -f file.hs -c config.yaml" <> fill'
    , indent 2 $ "Convert" <+> bold "HS" <+> "to" <+> bold "Markdown" <+> "using the config" <+> bold "config.yaml" <> colon <> fill'
    , indent 2 "file1.hs -> file1.hs.md"
    , fill'
    , bold "lima lhs2md -f file1.lhs -f file2.lhs" <> fill'
    , indent 2 $ "Convert" <+> bold "LHS" <+> "to" <+> bold "Markdown" <> colon <> fill'
    , indent 2 $ "file1.lhs -> file1.lhs.md" <> comma <-> "file2.lhs -> file2.lhs.md"
    ]

runLima :: IO Options
runLima =
  customExecParser
    (prefs (showHelpOnError <> disambiguate))
    ( info
        (helper <*> lima)
        ( fullDesc <> headerDoc (Just header_)
        )
    )

main :: IO ()
main = do
  Options{..} <- runLima
  Config{..} <-
    maybe'
      config
      def
      ( \c ->
          decodeFileEither c
            >>= ( \(x :: Either ParseException Config) ->
                    either' x (const $ error $ "Could not parse the config file at " <> c) pure
                )
      )
  contents_ <- forM files (\file -> readFile file `catch` (\(x :: SomeException) -> error $ "Could not read file at " <> file))
  let
    convert :: String -> FilePath -> IO ()
    convert contents out =
      (\(f, ext) -> writeFile (out <> "." <> ext) (f contents)) $
        case commandType of
          HS2MD -> maybe' configHs2Md (hsToMd def, "md") (\config_ -> (hsToMd config_, "md"))
          MD2LHS -> (mdToLhs, "lhs")
          LHS2MD -> (lhsToMd, "md")
  zipWithM_ convert contents_ files
  putStrLn "Converted!"
 where
  opts = info (lima <**> helper) mempty
  p = prefs (disambiguate <> showHelpOnError)