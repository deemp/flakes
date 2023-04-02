{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

main = print "hi"

-- import Control.Exception (SomeException, catch)
-- import Control.Monad (zipWithM_)
-- import Converter (
--   Config (..),
--   ConfigHsMd,
--   Dialect (..),
--   User,
--   demoConfig,
--   demoConfigHsMd,
--   translateTo,
--   -- hsToMd,
--   -- lhsToMd,
--   -- mdToHs,
--   -- mdToLhs,
--  )
-- import qualified Data.ByteString.Char8 as BSL
-- import Data.Default (def)
-- import Data.Foldable (Foldable (..))
-- import Data.List (intersperse)
-- import Data.Maybe (fromMaybe)
-- import Data.Traversable (forM)
-- import Data.Yaml (decodeFileThrow, encode)
-- import Options.Applicative
-- import Options.Applicative.Help (Doc, bold, comma, dot, fill, indent, lparen, rparen, softline, string, text, (<+>))
-- import Options.Applicative.Help.Pretty (hardline)

-- -- data FromTo = FromTo Dialect Dialect deriving (Show)

-- class ShowCommand a where
--   showCommand :: a -> String

-- -- instance ShowCommand CommandType where
-- --   showCommand :: CommandType -> String
-- --   showCommand = \case
-- --     Hs2Md -> "hs-md"
-- --     Md2Hs -> "md-hs"
-- --     Lhs2Md -> "lhs-md"
-- --     Md2Lhs -> "md-lhs"

-- data Lima = Lima

-- instance ShowCommand Lima where
--   showCommand :: Lima -> String
--   showCommand _ = "lima"

-- -- data Source = Hs | Md | Lhs

-- showSourceExtension :: Dialect -> String
-- showSourceExtension = \case
--   Hs -> "hs"
--   Md -> "md"
--   Lhs -> "lhs"
--   TeX -> "tex"

-- limaCommand :: Doc
-- limaCommand = string $ showCommand Lima

-- showToExtension :: FromTo -> String
-- showToExtension = showSourceExtension . to . commandToFromTo

-- data FromTo = FromTo {from :: Dialect, to :: Dialect}

-- -- commandToFromTo :: CommandType -> FromTo
-- -- commandToFromTo = \case
-- --   Hs2Md -> FromTo Hs Md
-- --   Md2Hs -> FromTo Md Hs
-- --   Lhs2Md -> FromTo Lhs Md
-- --   Md2Lhs -> FromTo Md Lhs

-- ppSource :: Dialect -> Doc
-- ppSource = \case
--   Hs -> "Haskell"
--   Md -> "Markdown"
--   Lhs -> "Literate Haskell"
--   TeX -> "TeX"

-- ppSourceBold :: Dialect -> Doc
-- ppSourceBold = bold . ppSource

-- data Options = Options
--   { fromDialect :: Dialect
--   , toDialect :: Dialect
--   , config :: Maybe FilePath
--   , files :: [FilePath]
--   }
--   deriving (Show)

-- parseConvertCommand :: Parser ([FilePath], Maybe FilePath)
-- parseConvertCommand = do
--   file <- some $ strOption (long "file" <> short 'f' <> metavar "FILE" <> help "Path to a file to convert")
--   config <- optional (strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Path to a config"))
--   return (file, config)

-- data Parsed
--   = ParsedOptions Options
--   | ParsedConfig (Config User)
--   | ParsedConfigHsMd (ConfigHsMd User)

-- mkConvertCommands :: FromTo -> Mod CommandFields Parsed
-- mkConvertCommands commandType =
--   command
--     (showCommand commandType)
--     ( info
--         ((\(files, config) -> ParsedOptions Options{..}) <$> parseConvertCommand)
--         (progDescDoc $ Just progDesc_)
--     )
--  where
--   fromLang = from . commandToFromTo $ commandType
--   toLang = to . commandToFromTo $ commandType
--   progDesc_ = "Convert" <-> bold (ppSource fromLang) <-> "to" <-> bold (ppSource toLang)

-- mkConfigCommand :: Mod CommandFields Parsed
-- mkConfigCommand =
--   command
--     "gen-config"
--     ( info
--         (pure $ ParsedConfig demoConfig)
--         (progDescDoc $ Just "Print a sample configuration.")
--     )

-- mkConfigHsMdCommand :: Mod CommandFields Parsed
-- mkConfigHsMdCommand =
--   command
--     "gen-config-hsMd"
--     ( info
--         (pure $ ParsedConfigHsMd demoConfigHsMd)
--         (progDescDoc $ Just $ "Print a sample" <-> bold "Haskell <-> Markdown" <-> "configuration.")
--     )

-- -- lima :: Parser (Maybe Options)
-- lima :: Parser Parsed
-- lima =
--   (subparser $ foldMap mkConvertCommands [Hs2Md, Md2Hs, Md2Lhs, Lhs2Md])
--     <|> (subparser $ mkConfigCommand)
--     <|> (subparser $ mkConfigHsMdCommand)

-- maybe' :: Maybe a -> b -> (a -> b) -> b
-- maybe' x f g = maybe f g x

-- descriptionBlock :: [Doc] -> Doc
-- descriptionBlock desc = fold (intersperse softline desc) <> hardline

-- (<->) :: Doc -> Doc -> Doc
-- x <-> y = x <> softline <> y

-- fill' :: Doc
-- fill' = fill 100 $ text ""

-- header_ :: Doc
-- header_ =
--   descriptionBlock
--     [ bold limaCommand <-> "converts between" <-> lparen <> boldHs <-> lparen <> bold ".hs" <> rparen
--     , "or" <-> boldLhs <> lparen <> bold ".lhs" <> rparen <> rparen
--     , "and" <-> boldMd <-> lparen <> bold ".md" <> rparen
--     , fill'
--     , "Learn more about a command by running:" <-> bold (limaCommand <+> "COMMAND")
--     , fill'
--     , "Example usage:" <> fill'
--     , fill'
--     , bold (limaCommand <+> command_ Hs2Md <+> "-f file.hs -c config.yaml") <> fill'
--     , indent 2 $ "Convert" <+> boldHs <+> "to" <+> boldMd <+> "using the config" <+> bold "config.yaml" <> dot <> fill'
--     , indent 2 $ "Result:" <> fill'
--     , indent 4 "file1.hs -> file1.hs.md"
--     , fill'
--     , bold (limaCommand <+> command_ Lhs2Md <+> "-f file1.lhs -f file2.lhs") <> fill'
--     , indent 2 $ "Convert" <+> boldLhs <+> "to" <+> boldMd <> dot <> fill'
--     , indent 2 $ "Result:" <> fill'
--     , indent 4 $ "file1.lhs -> file1.lhs.md" <> comma <-> "file2.lhs -> file2.lhs.md"
--     ]
--  where
--   boldHs = ppSourceBold Hs
--   boldMd = ppSourceBold Md
--   boldLhs = ppSourceBold Lhs
--   command_ = string . showCommand

-- runLima :: IO Parsed
-- runLima =
--   customExecParser
--     (prefs (showHelpOnError <> disambiguate))
--     ( info
--         (helper <*> lima)
--         ( fullDesc <> headerDoc (Just header_)
--         )
--     )

-- main :: IO ()
-- main = do
--   opts <- runLima
--   case opts of
--     ParsedConfig c -> BSL.putStrLn $ encode c
--     ParsedConfigHsMd c -> BSL.putStrLn $ encode c
--     ParsedOptions Options{..} -> do
--       Config{..} <-
--         maybe' config (pure $ def @(Config User)) $
--           \c ->
--             decodeFileThrow c
--               `catch` \(_ :: SomeException) ->
--                 error $ "Could not parse the config file at " <> c
--       contents_ <-
--         forM
--           files
--           ( \file ->
--               readFile file
--                 `catch` \(_ :: SomeException) ->
--                   error $ "Could not read file at " <> file
--           )
--       let
--         convert :: String -> FilePath -> IO ()
--         convert contents out =
--           (\(f, ext) -> writeFile (out <> "." <> ext) (f contents))
--             $ case commandType of
--               Md2Hs -> (mdToHs (fromMaybe def _hsMd),)
--               Hs2Md -> (hsToMd (fromMaybe def _hsMd),)
--               Md2Lhs -> (mdToLhs,)
--               Lhs2Md -> (lhsToMd,)
--             $ showToExtension commandType
--       zipWithM_ convert contents_ files
--       putStrLn "Converted!"