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

import Control.Exception (Exception, catch)
import Control.Exception.Base
  ( Exception (fromException),
    SomeException,
    handle,
    mask,
    throw,
    throwIO,
  )
import Control.Lens (At (at), Identity, filtered, non, over, withIndex, (^..), (^?!), _2)
import Control.Monad.Except
  ( MonadIO (liftIO),
    unless,
    when,
  )
import Control.Monad.Managed (MonadManaged, managed, runManaged)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap as KM (fromHashMapText)
import Data.Aeson.Lens (AsValue (..), key, members, _String)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as C
  ( cons,
    head,
    lines,
    null,
    tail,
    unlines,
  )
import Data.Char (isAlpha)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict as HM (fromList)
import Data.List (intercalate)
import Data.Maybe ()
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable ()
import Data.Vector as Vector ()
import Data.Yaml as Y (ParseException, decodeEither', encode)
import Filesystem.Path.CurrentOS as Path ()
import Options.Applicative
  ( Alternative ((<|>)),
    Parser,
    argument,
    command,
    commandGroup,
    customExecParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    prefs,
    progDesc,
    short,
    showHelpOnError,
    str,
    strOption,
    subparser,
    value,
  )
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectory, removeFile)
import System.FilePath (dropTrailingPathSeparator, isValid, pathSeparator, splitDirectories, (<.>), (</>))
import System.FilePath.Posix (takeDirectory)
import Prelude
import System.Exit (exitFailure)

-- Dealing with exceptions
-- http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html

-- Lenses etc.
-- https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references

-- Safe Resource handling
-- https://mmhaskell.com/blog/2022/6/23/resources-and-bracket

-- https://stackoverflow.com/a/51408207

main :: IO ()
main = do
  command' <-
    customExecParser
      (prefs showHelpOnError)
      ( info
          (helper <*> generalCommand)
          ( fullDesc
              <> header "manager - manage module and template files easily"
              <> progDesc "Organize Haskell files so that HLS accepts them"
          )
      )

  handleCommand command'
    `catch` ( \(ExMonoid x) -> do
                putStrLn ("The following problems occured:\n" <> intercalate "\n" (show <$> x))
                exitFailure
            )

  putStrLn "Done!"

modulesDir :: FilePath
modulesDir = "./Modules"

templatesDir :: FilePath
templatesDir = "./Templates"

packageYaml :: String
packageYaml = "./package.yaml"

managerDir :: FilePath
managerDir = "./manager"

ghci :: FilePath
ghci = "./.ghci"

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

data Target = TargetModule | TargetTemplate deriving (Show)

data GeneralCommand = GeneralCommand
  { target :: Target,
    command_ :: Command
  }
  deriving (Show)

data PathType = FileHs | TemplateHs | PackageYaml | Ghci | Directory

instance Show PathType where
  show :: PathType -> String
  show = \case
    FileHs -> "file"
    TemplateHs -> "template"
    PackageYaml -> packageYaml
    Ghci -> ".ghci"
    Directory -> "directory"

data ActionType = ERead | EWrite | ERemove

instance Show ActionType where
  show :: ActionType -> String
  show = \case
    ERead -> "reading"
    EWrite -> "writing"
    ERemove -> "removing"

data ProcessError
  = FileError
      { actionType :: ActionType,
        fileType :: PathType,
        filePath :: FilePath,
        message :: String
      }
  | NameError {name :: String}

instance Show ProcessError where
  show :: ProcessError -> String
  show FileError {..} = "Error" <-> show actionType <-> show fileType <-> qq filePath <-> ":" <-> message
  show NameError {name} =
    ("Error: the path or name" <-> qq name <-> "is bad.")
      <-> "It should be of form 'A(/A)*' like 'A' or 'A/A', where 'A' is an alphanumeric sequence."

instance Exception ProcessError

newtype Ex = Ex String deriving (Show)

instance Exception Ex

newtype ExMonoid = ExMonoid [SomeException]

instance Show ExMonoid where
  show :: ExMonoid -> String
  show (ExMonoid s) = show s

instance Exception ExMonoid

eitherTarget :: p -> p -> Target -> p
eitherTarget f g x =
  case x of
    TargetModule -> f
    TargetTemplate -> g

makeSubCommand :: Target -> Parser Command
makeSubCommand target =
  subparser
    ( f
        "add"
        addCommand
        ( ("Add a" <-> name <-> "at '" <> dir </> "NAME.hs'. It will also appear in the './package.yaml'.")
            <-> "A NAME should be of form 'A(/A)*', like 'A' or 'A/A', where 'A' is an alphanumeric sequence."
            <-> "A NAME 'A/A' refers to a"
            <-> name
            <-> "at"
            <-> qq (dir </> "A/A.hs")
        )
        <> f
          "rm"
          removeCommand
          ( ("Remove a" <-> name <-> "at '" <> dir </> "NAME.hs' and its empty parent directories.")
              <-> ("This" <-> name <-> "will also be removed from './package.yaml'")
          )
        <> f
          "list"
          listCommand
          ( "List " <> name <> "s available in './package.yaml'"
          )
        <> f
          "set"
          setCommand
          ( ("Set a " <> name <> " so that it's loaded when a 'ghci' session starts.")
              <-> ("When 'ghcid' starts, it will run this " <> name <> "'s 'main'")
          )
    )
  where
    f name' command' desc = command name' (info (helper <*> command') (fullDesc <> progDesc desc))
    (dir, name) =
      case target of
        TargetModule -> (modulesDir, "module")
        TargetTemplate -> (templatesDir, "template")

templatesSubCommand :: Parser Command
templatesSubCommand = makeSubCommand TargetTemplate

modulesSubCommand :: Parser Command
modulesSubCommand = makeSubCommand TargetModule

generalCommand :: Parser GeneralCommand
generalCommand =
  -- TODO some description for modules command
  (GeneralCommand TargetModule <$> modulesSubCommand)
    <|> GeneralCommand TargetTemplate <$> subparser (f "template" templatesSubCommand "Manipulate templates" <> commandGroup "Template commands:")
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
parseTemplate =
  strOption
    ( long "template"
        <> short 't'
        <> metavar "NAME"
        <> value defaultTemplate
        <> help "A template NAME"
    )

addCommand :: Parser Command
addCommand = CommandAdd <$> parseFileName <*> parseTemplate

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseFileName

-- | concatenate strings with a space
(<->) :: (IsString a, Semigroup a) => a -> a -> a
x <-> y = x <> " " <> y

-- | enclose a string into single quotes
qq :: (IsString a, Semigroup a) => a -> a
qq s = "'" <> s <> "'"

renderProcessError :: ProcessError -> Text
renderProcessError = T.pack . show

do' :: String -> IO ()
do' x = putStrLn ("do" <-> x)

undo' :: String -> IO ()
undo' x = putStrLn ("undo" <-> x)

fmapE :: forall e a. Exception e => (e -> a) -> ExMonoid -> [Maybe a]
fmapE f (ExMonoid s) = (f <$>) . fromException <$> s

filterMapE :: forall e a. Exception e => (e -> a) -> ExMonoid -> [a]
filterMapE f (ExMonoid s) = Prelude.foldr (\x m -> maybe m ((: m) . f) (fromException @e x)) [] s

bracketOnError' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError' before after thing =
  mask $ \restore ->
    -- in case the 'before' action throws
    catchThrow
      ( do
          a <- before
          catchThrow (restore (thing a))
            `onException'` after a
      )
  where
    catchThrow y = y `catch` (\(x :: SomeException) -> throwIO $ ExMonoid $ maybe [x] (\(ExMonoid x') -> x') (fromException x))

-- | perform an action A
-- if it causes an exception, perform an action B
-- collect exceptions of both actions into a monoid
onException' :: IO a -> IO b -> IO a
onException' io what =
  io
    `catch` ( \x'@(ExMonoid x) -> do
                _ <- what `catch` (\y -> throwIO $ ExMonoid (y : x))
                throwIO x'
            )

-- | managed bracketOnError' that should restore in case of errors
mBracketOnError :: MonadManaged m => IO a -> (a -> IO b) -> m a
mBracketOnError x y = managed (bracketOnError' x y)

-- | managed bracketOnError' that shouldn't restore in case of errors
mWith :: MonadManaged m => IO a -> m a
mWith x = mBracketOnError x (const (return ()))

-- | demo collect exceptions into a monoid
tryManaged' :: IO ()
tryManaged' = handle (\(x :: ExMonoid) -> print $ filterMapE @Ex (\(Ex t) -> "Print " <> t) x) $
  runManaged $ do
    _ <- mBracketOnError (do' "A" >> throw (Ex "Arelease")) (\_ -> undo' "A" >> throw (Ex "Arelease"))
    _ <- mBracketOnError (do' "B") (\_ -> undo' "B" >> throw (Ex "B"))
    _ <- mBracketOnError (do' "C") (\_ -> undo' "C" >> throw (Ex "C"))
    _ <- mBracketOnError (do' "D" >> throw (Ex "Da")) (\_ -> undo' "D" >> throw (Ex "Dr"))
    liftIO $ throw $ Ex "final1"

-- | safely handle command
-- collect into a monoid and rethrow the exceptions that occur when doing or undoing actions
handleCommand :: GeneralCommand -> IO ()
handleCommand (GeneralCommand {..}) = runManaged $ case command_ of
  CommandAdd {name, template} -> do
    throwIfBadName name
    throwIfBadName template
    liftIO $ putStrLn $ "Reading template at" <-> qq templateHs
    t <- mWith ((BS.readFile templateHs `catchThrow`) ERead TemplateHs templateHs)
    liftIO $ putStrLn $ "Writing the template into" <-> qq fileHs
    mBracketOnError
      ((createDirectoryIfMissing True targetDir `catchThrow`) EWrite Directory fileHs)
      (\_ -> (removeFile fileHs `catchThrow`) ERemove Directory fileHs)
    mBracketOnError
      ((BS.writeFile fileHs t `catchThrow`) EWrite FileHs fileHs)
      (\_ -> (removeFile fileHs `catchThrow`) ERemove FileHs fileHs)
    readPackageYaml $ \y1 atKey nempty -> do
      let y2 =
            y1
              & over
                (key executables . atKey (mkExe name) . nempty . atKey main')
                (\_ -> Just $ String (T.pack fileHs))
      writePackageYaml y2
    where
      fileHs = mkTargetHs name
      templateHs = templatesDir </> template <.> "hs"
      targetDir = takeDirectory fileHs
  CommandList -> do
    readPackageYaml $ \y1 _ _ -> do
      liftIO $ putStrLn "Executables:"
      let check x =
            (\(a : b : _) -> a </> b) (splitDirectories (T.unpack x))
              `Prelude.notElem` [managerDir, targetTopDirComplementary]
          y2 =
            (y1 ^.. key executables . members . filtered (\x -> check (x ^?! key main' . _String)) . withIndex)
              <&> over _2 (\y -> y ^?! key main' . _String)
      traverse_ (\(name, path) -> liftIO $ putStrLn $ T.unpack $ name <-> "(" <-> path <-> ")") y2
  CommandRemove {name} -> do
    throwIfBadName name
    t <- mWith ((BS.readFile fileHs `catchThrow`) ERead FileHs fileHs)
    liftIO $ putStrLn $ "Removing" <-> qq fileHs
    mBracketOnError
      ((removeFile fileHs `catchThrow`) ERemove FileHs fileHs)
      -- restore the contents
      (\_ -> (BS.writeFile fileHs t `catchThrow`) EWrite FileHs fileHs)
    mBracketOnError
      ((traverse_ removeDirectoryIfEmpty targetDirParents `catchThrow`) ERemove Directory targetDir)
      -- restore the contents
      (\_ -> (createDirectoryIfMissing True targetDir `catchThrow`) EWrite Directory targetDir)
    readPackageYaml $ \y1 _ _ -> do
      let y2 =
            y1
              & over
                (key executables)
                ( \x ->
                    Object $
                      KM.fromHashMapText $
                        HM.fromList
                          ( x ^.. members . withIndex . filtered (\(y, _) -> y /= exe)
                          )
                )
      writePackageYaml y2
    where
      targetDir = takeDirectory fileHs
      targetDirParents = take (length (splitDirectories targetDir)) (iterate takeDirectory targetDir)
      fileHs = mkTargetHs name
      exe = mkExe name
  CommandSet {name} -> do
    throwIfBadName name
    mWith ((Prelude.writeFile ghci txt `catchThrow`) EWrite Ghci ghci)
    where
      txt = ":set -isrc\n:load" <-> mkTargetHs name
  where
    main' = "main"
    executables = "executables"
    targetTopDir = eitherTarget modulesDir templatesDir target
    targetTopDirComplementary = eitherTarget templatesDir modulesDir target
    mkTargetHs x = targetTopDir </> x <.> "hs"
    isOkName name = isValid name && hasNoTrailingSeparator && all (all (`elem` alphabet)) (splitDirectories name)
      where
        hasNoTrailingSeparator = dropTrailingPathSeparator name == name
        alphabet = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']
    throwIfBadName name = mWith (liftIO $ unless (isOkName name) (throwIO $ NameError name))
    removeDirectoryIfEmpty dir = do
      isEmptyDir <- Prelude.null <$> listDirectory dir
      when isEmptyDir (removeDirectory dir)
    mkExe x =
      T.pack $
        eitherTarget "" "Templates." target
          <> ((\y -> if y == pathSeparator then '.' else y) <$> x)
    -- safely read a package.yaml and run the continuation ok if everything is OK
    readPackageYaml ok = do
      liftIO $ putStrLn $ "Reading" <-> qq packageYaml
      y1 <-
        mBracketOnError
          ((BS.readFile packageYaml `catchThrow`) ERead PackageYaml packageYaml)
          -- restore the contents
          (\x -> (BS.writeFile packageYaml x `catchThrow`) EWrite PackageYaml packageYaml)
      let (y2 :: Either ParseException Value) = decodeEither' y1
      case y2 of
        Left l -> liftIO $ throwIO $ FileError ERead PackageYaml packageYaml (show l)
        Right r ->
          let atKey :: Text -> (Maybe Value -> Identity (Maybe Value)) -> Value -> Identity Value
              atKey k = _Object . at k
              nempty :: (Value -> Identity Value) -> Maybe Value -> Identity (Maybe Value)
              nempty = non (Object mempty)
           in ok r atKey nempty
    -- safely write package.yaml
    writePackageYaml y1 = do
      liftIO $ putStrLn $ "Updating" <-> qq packageYaml
      let y2 =
            C.lines (Y.encode y1)
              <&> (\x -> if not (C.null x) && isAlpha (C.head x) then C.cons '\n' x else x)
              & C.unlines
              & C.tail
      -- we'll anyway restore content on an exceptio
      mWith ((BS.writeFile packageYaml y2 `catchThrow`) EWrite PackageYaml packageYaml)
    -- translate an exception into a custom exception for easier handling
    catchThrow :: IO a -> ActionType -> PathType -> FilePath -> IO a
    catchThrow x actionType fileType filePath =
      x `catch` (\(e :: SomeException) -> throwIO $ FileError actionType fileType filePath (show e))