{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Error (handleExceptT, hoistEither, isLeft, throwE)

{- stack
  script
  --resolver lts-19.21
  --package mtl
  --package array
  --package system-filepath
  --package aeson
  --package bytestring
  --package text
  --package directory
  --package optparse-applicative
  --package errors
  --package yaml
  --package unordered-containers
  --package lens-aeson
  --package lens
  --package pretty-show
  --package filepath
-}

import Control.Exception (catch)
import Control.Exception.Base
import Control.Lens (At (at), filtered, has, hasn't, indices, non, only, over, (%=), (%~), (.~), (?~), (^.), (^..), (^?), (^?!))
import Control.Monad (unless, when)
import Control.Monad.Except
import Data.Aeson (ToJSON (toJSON), Value (..), encode, (.:), (.=))
import qualified Data.Aeson as JSON
import Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (AsNumber (_Number), AsValue (..), key, members, values, _String)
import Data.ByteString (readFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import Data.Function ((&))
import Data.HashMap.Strict as HM
import Data.List ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ()
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Traversable ()
import Data.Vector as Vector
import Data.Yaml (FromJSON, Object, ParseException, Value (Object), decodeEither', decodeFileThrow)
import Filesystem.Path.CurrentOS as Path ()
import GHC.Generics (Generic)
import Options.Applicative (Parser, ParserInfo, argument, command, customExecParser, fullDesc, header, help, helper, info, long, metavar, optional, prefs, progDesc, short, showHelpOnError, str, strOption, subparser, value)
import System.Directory
import System.FilePath.Posix (takeDirectory)
import Text.Show.Pretty (pPrint, ppShow)
import Prelude

-- Dealing with exceptions
-- http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html

-- Lenses etc.
-- https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references

-- Safe Resource handling
-- https://mmhaskell.com/blog/2022/6/23/resources-and-bracket

-- https://stackoverflow.com/a/51408207

data Command
  = CommandList
  | CommandAdd
      { name :: String,
        template :: String
      }
  | CommandRemove
      { name :: String
      }
  | CommandSet
      { name :: String
      }
  deriving (Show)

moduleSubCommand :: Parser Command
moduleSubCommand =
  subparser
    ( f "add" addCommand "Add a module"
        <> f "rm" removeCommand "Remove a module"
        <> f "list" listCommand "List modules"
        <> f "set" setCommand "Set current module for ghcid"
    )
  where
    f name' command' desc = command name' (info (helper <*> command') (fullDesc <> progDesc desc))

defaultTemplate :: String
defaultTemplate = "tools/data/template.hs"

setCommand :: Parser Command
setCommand = CommandSet <$> parseFileName

listCommand :: Parser Command
listCommand = pure CommandList

parseFileName :: Parser String
parseFileName = argument str (metavar "FILE")

parseTemplate :: Parser String
parseTemplate =
  strOption
    ( long "template" <> short 't' <> metavar "FILE" <> value defaultTemplate <> help "A template FILE"
    )

addCommand :: Parser Command
addCommand = CommandAdd <$> parseFileName <*> parseTemplate

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseFileName

main :: IO ()
main = do
  command_ <-
    customExecParser
      (prefs showHelpOnError)
      ( info
          (helper <*> moduleSubCommand)
          ( fullDesc
              <> header "module - manage project modules easily"
              <> progDesc "Manage modules in this project so that HLS accepts them"
          )
      )
  print command_

  h <- runExceptT $ handleCommand command_
  either (pPrint . renderProcessError) return h

data FileType = File | Template | Hie | Package

instance Show FileType where
  show = \case
    File -> "file"
    Template -> "template"
    Hie -> hie
    Package -> package

data ProcessError
  = ERead FileType FilePath String
  | EWrite FileType FilePath String

renderProcessError :: ProcessError -> Text
renderProcessError pe = T.pack $
  case pe of
    ERead t p e -> "Error reading " <> show t <> " '" <> p <> "' : " <> e
    EWrite t p e -> "Error reading " <> show t <> " '" <> p <> "' : " <> e

defaultDir = "Contest"

hie = "hie.yaml"

package = "package.yaml"

handleCommand :: Command -> ExceptT ProcessError IO ()
handleCommand = \case
  CommandAdd {..} -> do
    liftIO $ putStrLn $ "Reading template at " <> template
    t <- handleExceptT handleReadTemplate $ BS.readFile template
    liftIO $ putStrLn $ "Writing the template into '" <> path <> "'"
    handleExceptT handleWriteTemplate $ do
      createDirectoryIfMissing True defaultDir
      BS.writeFile path t
    liftIO $ putStrLn $ "Reading '" <> hie <> "'"
    (y1 :: Value) <- handleExceptT handleReadHie $ decodeFileThrow hie
    let newHieEntry = Object (KM.fromList [("component", String . T.pack $ "acpoj:exe:" <> name <> "-exe"), ("path", String pathText)])
        y2 = y1 & over (key "cradle" . key "stack") (
          \x -> Array $ Vector.fromList $ (
            x ^.. values . filtered (hasn't (key "path" . _String . only pathText))) <> [newHieEntry])
    liftIO $ print $ encode y2
    where
      path = "./" <> defaultDir <> "/" <> name <> ".hs"
      pathText = T.pack path

      handleReadTemplate :: SomeException -> ProcessError
      handleReadTemplate e = ERead Template template (show e)

      handleWriteTemplate :: SomeException -> ProcessError
      handleWriteTemplate e = EWrite Template path (show e)

      handleReadHie :: SomeException -> ProcessError
      handleReadHie e = ERead Hie path (show e)
  CommandList -> undefined
  CommandRemove {..} -> undefined
  CommandSet {..} -> undefined
