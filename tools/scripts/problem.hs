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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Error (handleExceptT, hoistEither, isLeft, throwE)
import Control.Exception (catch)
import Control.Exception.Base
import Control.Lens (At (at), filtered, has, hasn't, indices, non, only, over, (%=), (%~), (.~), (?~), (^.), (^..), (^?), (^?!))
import Control.Monad (unless, when)
import Control.Monad.Cont (ContT)
import Control.Monad.Except
import Control.Monad.Managed (managed, runManaged)
import Data.Aeson (ToJSON (toJSON), Value (..), (.:), (.=))
import qualified Data.Aeson as JSON
import Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (AsNumber (_Number), AsValue (..), key, members, values, _String)
import Data.ByteString (readFile)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C hiding (putStrLn)
import Data.ByteString.Lazy (toStrict)
import Data.Char (isAlpha)
import Data.Either (fromRight)
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

defaultDir, package :: String
defaultDir = "Contest"
package = "package.yaml"

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
defaultTemplate = "Templates/Contest.hs"

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

data FileType = File | Template | Package

instance Show FileType where
  show = \case
    File -> "file"
    Template -> "template"
    Package -> package

data ProcessError
  = ERead FileType FilePath String
  | EWrite FileType FilePath String

instance Show ProcessError where
  show = \case
    ERead t p e -> "Error reading " <> show t <> " '" <> p <> "' : " <> e
    EWrite t p e -> "Error reading " <> show t <> " '" <> p <> "' : " <> e

renderProcessError :: ProcessError -> Text
renderProcessError pe = T.pack $
  case pe of
    ERead t p e -> "Error reading " <> show t <> " '" <> p <> "' : " <> e
    EWrite t p e -> "Error reading " <> show t <> " '" <> p <> "' : " <> e

do' x = putStrLn ("do " <> x)

undo' :: String -> IO ()
undo' x = putStrLn ("undo " <> x)

newtype MyEx = MyEx ProcessError deriving (Show)

instance Exception MyEx

newtype Ex = Ex String deriving (Show)

instance Exception Ex

data ExMonoid = forall e. Exception e => ExMonoid [e]

-- getExs :: Exception e => ExMonoid -> [Maybe e]
-- getExs (ExMonoid em) = fromException <$> em

-- exs :: [Maybe Ex]
-- exs = getExs $ ExMonoid [SomeException (Ex "a"), SomeException (ExMonoid [Ex "a"])]

{-
>>>exs
[Nothing,Nothing]
-}

instance Show ExMonoid where
  show (ExMonoid s) = show s

instance Exception ExMonoid

bracketOnError' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError' before after thing =
  mask $ \restore -> do
    a <- before
    (restore (thing a) `catch` (\(x :: SomeException) -> throwIO $ ExMonoid [x])) `onException'` after a

onException' :: IO a -> IO b -> IO a
onException' io what = io `catch` (\x'@(ExMonoid x) -> do _ <- what `catch` (\y -> throwIO $ ExMonoid (y : x)); throwIO x')
-- onException' io what = io `catch` (\x'@(ExMonoid x) -> do _ <- what `catch` (\y -> throwIO $ ExMonoid (y : x)); throwIO x')


-- throw' :: Exception e => e -> a
throw' :: Exception e => e -> IO a
throw' e = throwIO $ ExMonoid [e]

tryManaged' :: IO ()
tryManaged' = runManaged $ do
  _ <- managed (bracketOnError' (do' "A") (\_ -> undo' "A" >> throw (Ex "A")))
  _ <- managed (bracketOnError' (do' "B") (\_ -> undo' "B" >> throw (Ex "B")))
  _ <- managed (bracketOnError' (do' "C") (\_ -> undo' "C" >> throw (Ex "C")))
  _ <- managed (bracketOnError' (do' "D" >> throw (Ex "Da")) (\_ -> undo' "D" >> throw (Ex "Dr")))
  liftIO $ throw $ Ex "final1"

tryManaged :: IO ()
tryManaged = runManaged $ do
  _ <- managed (bracket (do' "A") (\_ -> undo' "A" >> throw (Ex "A")))
  _ <- managed (bracket (do' "B") (\_ -> undo' "B" >> throw (Ex "B")))
  liftIO $ throw $ Ex "final"

-- handleCommandAdd = runManaged $ runWriterT

handleCommandCPS :: Command -> IO ()
handleCommandCPS = \case
  CommandAdd {..} -> runManaged $ do
    liftIO $ putStrLn $ "Reading template at " <> template
    -- I'm not sure I should throw an exception here
    -- TODO an exception may come either from reading a file or from using it or from a further exception
    t <- liftIO $ BS.readFile template `catch` (\(h :: SomeException) -> throw $ MyEx (ERead Package template (show h)))
    liftIO $ putStrLn $ "Writing the template into '" <> path <> "'"
    liftIO $ createDirectoryIfMissing True defaultDir
    f <- managed (bracketOnError (openFile path WriteMode) (\_ -> removeFile path))
    liftIO $ BS.hPut f t >> hClose f
    liftIO $ putStrLn $ "Reading '" <> package <> "'"
    -- We need to restore the original file contents in case of future errors
    -- We pretend that there will be no exceptions while writing the file
    -- We want to collect the exceptions - some when moving forwards, and some when restoring the state
    -- TODO not just print about an exception, rather put it into a monoid
    y1 <- managed (bracketOnError (BS.readFile package) (\x -> BS.writeFile package x `catch` (\(e :: SomeException) -> print $ ERead Package package (show e))))
    let (y2 :: Either ParseException Value) = decodeEither' y1
    case y2 of
      Left e -> liftIO $ throw $ MyEx (ERead Package package (show e))
      Right r -> do
        let atKey k = _Object . at k
            nempty = non (Object mempty)
            y3 = r & over (key "executables" . atKey (T.pack name <> "-exe") . nempty . atKey "main") (\_ -> Just $ String pathText)
            y4 = C.lines (Y.encode y3) <&> (\x -> if not (C.null x) && isAlpha (C.head x) then C.cons '\n' x else x) & C.unlines & C.tail
        -- either write the new text or restore the initial one
        -- TODO writeFile doesn't look like resource acquisition
        -- yf <- managed (bracketOnError (openFile package WriteMode) (\h -> _))
        return ()

    return ()
    where
      -- (y1 :: Value) <- handleExceptT handleReadPackage $ decodeFileThrow package
      -- let atKey k = _Object . at k
      --     nempty = non (Object mempty)
      --     y2 = y1 & over (key "executables"  . atKey (T.pack name <> "-exe") . nempty  . atKey "main") (\_ -> Just $ String pathText)
      --     enc =  C.lines (Y.encode y2) <&> (\x -> if not (C.null x) && isAlpha (C.head x) then C.cons '\n' x else x) & C.unlines & C.tail
      -- liftIO $ putStrLn $ "Writing '" <> package <> "'"
      -- handleExceptT handleWriteTemplate $ BS.writeFile package enc

      path = "./" <> defaultDir <> "/" <> name <> ".hs"
      pathText = T.pack path

      handleReadTemplate :: SomeException -> ProcessError
      handleReadTemplate e = ERead Template template (show e)

      handleWriteTemplate :: SomeException -> ProcessError
      handleWriteTemplate e = EWrite Template path (show e)

      handleReadPackage :: SomeException -> ProcessError
      handleReadPackage e = ERead Package path (show e)
  _ -> liftIO $ return ()

handleCommand :: Command -> ExceptT ProcessError IO ()
handleCommand = \case
  CommandAdd {..} -> do
    liftIO $ putStrLn $ "Reading template at " <> template
    t <- handleExceptT handleReadTemplate $ BS.readFile template
    liftIO $ putStrLn $ "Writing the template into '" <> path <> "'"
    handleExceptT handleWriteTemplate $ do
      createDirectoryIfMissing True defaultDir
      BS.writeFile path t
    liftIO $ putStrLn $ "Reading '" <> package <> "'"
    (y1 :: Value) <- handleExceptT handleReadPackage $ decodeFileThrow package
    let atKey k = _Object . at k
        nempty = non (Object mempty)
        y2 = y1 & over (key "executables" . atKey (T.pack name <> "-exe") . nempty . atKey "main") (\_ -> Just $ String pathText)
        enc = C.lines (Y.encode y2) <&> (\x -> if not (C.null x) && isAlpha (C.head x) then C.cons '\n' x else x) & C.unlines & C.tail
    liftIO $ putStrLn $ "Writing '" <> package <> "'"
    handleExceptT handleWriteTemplate $ BS.writeFile package enc
    where
      path = "./" <> defaultDir <> "/" <> name <> ".hs"
      pathText = T.pack path

      handleReadTemplate :: SomeException -> ProcessError
      handleReadTemplate e = ERead Template template (show e)

      handleWriteTemplate :: SomeException -> ProcessError
      handleWriteTemplate e = EWrite Template path (show e)

      handleReadPackage :: SomeException -> ProcessError
      handleReadPackage e = ERead Package path (show e)
  CommandList -> undefined
  CommandRemove {..} -> undefined
  CommandSet {..} -> undefined
