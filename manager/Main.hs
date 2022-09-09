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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant section" #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Error (handleExceptT, hoistEither, isLeft, throwE)
import Control.Exception (catch)
import Control.Exception.Base
import Control.Lens (At (at), Identity, Profunctor, filtered, has, hasn't, indices, non, only, over, withIndex, (%=), (%~), (.~), (?~), (^.), (^..), (^?), (^?!), _2)
import Control.Monad (unless, when)
import Control.Monad.Cont (ContT)
import Control.Monad.Except
import Control.Monad.Managed (MonadManaged, managed, runManaged)
import Data.Aeson (ToJSON (toJSON), Value (..), (.:), (.=))
import qualified Data.Aeson as JSON
import Data.Aeson.KeyMap as KM hiding (traverse)
import Data.Aeson.Lens (AsNumber (_Number), AsValue (..), key, members, values, _String)
import Data.ByteString (readFile)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C hiding (putStrLn)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAlpha)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
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
import Data.Yaml as Y (FromJSON, Object, ParseException, Value (Object), decodeEither', decodeFileThrow, encode, encodeFile)
import Filesystem.Path.CurrentOS as Path ()
import GHC.Generics (Generic)
import Options.Applicative (Parser, ParserInfo, argument, command, customExecParser, fullDesc, header, help, helper, info, long, metavar, optional, prefs, progDesc, short, showHelpOnError, str, strOption, subparser, value)
import System.Directory
import System.FilePath.Posix (takeDirectory)
import System.IO (IOMode (WriteMode), hClose, openFile)
import Text.Show.Pretty (pPrint, ppShow)
import Prelude

-- Dealing with exceptions
-- http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html

-- Lenses etc.
-- https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references

-- Safe Resource handling
-- https://mmhaskell.com/blog/2022/6/23/resources-and-bracket

-- https://stackoverflow.com/a/51408207

modulesDir, templatesDir, packageYaml :: FilePath
modulesDir = "./Modules"
templatesDir = "./templates"
packageYaml = "./package.yaml"

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
    ( f "add" addCommand "Add a module at './Modules/NAME.hs'. It will be added to the './package.yaml'"
        <> f "rm" removeCommand "Remove a module at './Modules/NAME.hs'"
        <> f "list" listCommand "List modules available in './package.yaml'"
        <> f "set" setCommand "Set a module so that it's loaded when a 'ghci'-session starts. When 'ghcid' starts, it will run this module's 'main'"
        -- TODO add subcommand for creating templates
        -- problem template add/rm/list
    )
  where
    f name' command' desc = command name' (info (helper <*> command') (fullDesc <> progDesc desc))

defaultTemplate :: String
defaultTemplate = "Contest"

setCommand :: Parser Command
setCommand = CommandSet <$> parseFileName

listCommand :: Parser Command
listCommand = pure CommandList

parseFileName :: Parser String
parseFileName = argument str (metavar "NAME")

parseTemplate :: Parser String
parseTemplate = strOption (long "template" <> short 't' <> metavar "NAME" <> value defaultTemplate <> help "A template NAME")

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
  -- print command_

  -- todo handle exceptions
  h <- handleCommand command_
  putStrLn "Done!"

-- either (pPrint . renderProcessError) return h

data FileType = FileHs | TemplateHs | PackageYaml | Ghci

instance Show FileType where
  show = \case
    FileHs -> "file"
    TemplateHs -> "template"
    PackageYaml -> packageYaml
    Ghci -> ".ghci"

data ErrorType = ERead | EWrite | ERemove

instance Show ErrorType where
  show = \case
    ERead -> "reading"
    EWrite -> "writing"
    ERemove -> "removing"

data ProcessError = ProcessError
  { errorType :: ErrorType,
    fileType :: FileType,
    filePath :: FilePath,
    message :: String
  }

instance Show ProcessError where
  show ProcessError {..} = "Error " <> show errorType <> " " <> show fileType <> " '" <> filePath <> "' : " <> message

renderProcessError :: ProcessError -> Text
renderProcessError = T.pack . show

do' x = putStrLn ("do " <> x)

undo' :: String -> IO ()
undo' x = putStrLn ("undo " <> x)

instance Exception ProcessError

-- newtype MyEx = MyEx ProcessError deriving (Show)

-- instance Exception MyEx

newtype Ex = Ex String deriving (Show)

instance Exception Ex

newtype ExMonoid = ExMonoid [SomeException]

instance Show ExMonoid where
  show :: ExMonoid -> String
  show (ExMonoid s) = show s

instance Exception ExMonoid

fmapE :: forall e a. Exception e => (e -> a) -> ExMonoid -> [Maybe a]
fmapE f (ExMonoid s) = (f <$>) . fromException <$> s

filterMapE :: forall e a. Exception e => (e -> a) -> ExMonoid -> [a]
filterMapE f (ExMonoid s) = Prelude.foldr (\x m -> maybe m ((: m) . f) (fromException @e x)) [] s

bracketOnError' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError' before after thing =
  mask $ \restore -> do
    a <- before
    ( restore (thing a)
        `catch` ( \(x :: SomeException) ->
                    case fromException x of
                      Just (ExMonoid x') -> throwIO $ ExMonoid x'
                      _ -> throwIO $ ExMonoid [x]
                )
      )
      `onException'` after a

onException' :: IO a -> IO b -> IO a
onException' io what = io `catch` (\x'@(ExMonoid x) -> do _ <- what `catch` (\y -> throwIO $ ExMonoid (y : x)); throwIO x')

mBracketOnError :: MonadManaged m => IO a -> (a -> IO b) -> m a
mBracketOnError x y = managed (bracketOnError' x y)

mWith :: MonadManaged m => IO a -> m a
mWith x = mBracketOnError x (const (return ()))

tryManaged' :: IO ()
tryManaged' = handle (\(x :: ExMonoid) -> print $ filterMapE @Ex (\(Ex t) -> "Print " <> t) x) $
  runManaged $ do
    _ <- managed (bracketOnError' (do' "A") (\_ -> undo' "A" >> throw (Ex "Arelease")))
    _ <- managed (bracketOnError' (do' "B") (\_ -> undo' "B" >> throw (Ex "B")))
    _ <- managed (bracketOnError' (do' "C") (\_ -> undo' "C" >> throw (Ex "C")))
    _ <- managed (bracketOnError' (do' "D" >> throw (Ex "Da")) (\_ -> undo' "D" >> throw (Ex "Dr")))
    liftIO $ throw $ Ex "final1"

handleCommand :: Command -> IO ()
handleCommand c = runManaged $ case c of
  CommandAdd {name, template} -> do
    liftIO $ putStrLn $ "Reading template at '" <> templateHs <> "'"
    t <- mWith ((BS.readFile templateHs `catch'`) ERead TemplateHs templateHs)
    liftIO $ putStrLn $ "Writing the template into '" <> fileHs <> "'"
    liftIO $ createDirectoryIfMissing True modulesDir
    mBracketOnError
      ((BS.writeFile fileHs t `catch'`) EWrite FileHs fileHs)
      (\_ -> (removeFile fileHs `catch'`) ERemove FileHs fileHs)
    readPackageYaml $ \y1 atKey nempty -> do
      let y2 = y1 & over (key executables . atKey (mkExe name) . nempty . atKey main') (\_ -> Just $ String (T.pack fileHs))
      writePackageYaml y2
    where
      fileHs = mkFileHs name
      templateHs = templatesDir <> "/" <> template <> ".hs"
  CommandList -> do
    readPackageYaml $ \y1 _ _ -> do
      liftIO $ putStrLn "Executables:"
      let check x = takeDirectory (T.unpack x) `Prelude.notElem` ["./tools/scripts", templatesDir]
          y2 = (y1 ^.. key executables . members . filtered (\x -> check (x ^?! key main' . _String)) . withIndex) <&> over _2 (\y -> y ^?! key main' . _String)
      traverse_ (\(name, path) -> liftIO $ putStrLn $ T.unpack $ name <> " ( " <> path <> " )") y2
  CommandRemove {name} -> do
    t <- mWith ((BS.readFile fileHs `catch'`) ERead FileHs fileHs)
    liftIO $ putStrLn $ "Removing '" <> fileHs <> "'"
    mBracketOnError
      ((removeFile fileHs `catch'`) ERemove FileHs fileHs)
      -- restore the contents
      (\_ -> (BS.writeFile fileHs t `catch'`) EWrite FileHs fileHs)
    readPackageYaml $ \y1 _ _ -> do
      let y2 = y1 & over (key executables) (\x -> Object $ KM.fromHashMapText $ HM.fromList (x ^.. members . withIndex . filtered (\(y, _) -> y /= exe)))
      writePackageYaml y2
    where
      fileHs = mkFileHs name
      exe = mkExe name
  CommandSet {name} -> do
    mWith ((Prelude.writeFile ghci txt `catch'`) EWrite Ghci ghci)
    where
      ghci = "./.ghci"
      txt = ":set -isrc\n:load " <> mkFileHs name
  where
    main' = "main"
    executables = "executables"
    mkExe x = T.pack x
    mkFileHs x = modulesDir <> "/" <> x <> ".hs"
    readPackageYaml ok = do
      liftIO $ putStrLn $ "Reading '" <> packageYaml <> "'"
      y1 <-
        mBracketOnError
          ((BS.readFile packageYaml `catch'`) ERead PackageYaml packageYaml)
          -- restore the inital contents
          (\x -> (BS.writeFile packageYaml x `catch'`) EWrite PackageYaml packageYaml)
      let (y2 :: Either ParseException Value) = decodeEither' y1
      case y2 of
        Left e -> liftIO $ throwIO $ ProcessError ERead PackageYaml packageYaml (show e)
        Right r ->
          let atKey :: Text -> (Maybe Value -> Identity (Maybe Value)) -> Value -> Identity Value
              atKey k = _Object . at k
              nempty :: (Value -> Identity Value) -> Maybe Value -> Identity (Maybe Value)
              nempty = non (Object mempty)
           in ok r atKey nempty
    writePackageYaml y1 = do
      liftIO $ putStrLn $ "Updating '" <> packageYaml <> "'"
      let y2 = C.lines (Y.encode y1) <&> (\x -> if not (C.null x) && isAlpha (C.head x) then C.cons '\n' x else x) & C.unlines & C.tail
      mBracketOnError
        ((BS.writeFile packageYaml y2 `catch'`) EWrite PackageYaml packageYaml)
        -- we'll write the initial contents anyway
        (\_ -> return ())
    catch' x errorType fileType filePath = x `catch` (\(e :: SomeException) -> throwIO $ ProcessError errorType fileType filePath (show e))
