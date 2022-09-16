module Main (main) where

import Control.Exception (Exception, catch)
import Control.Exception.Base (SomeException, throwIO)
import Control.Lens (At (at), Identity, filtered, non, over, withIndex, (^..), (^?!), _2)
import Control.Monad.Except (MonadIO (liftIO), unless)
import Control.Monad.Managed (runManaged)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap as KM (fromHashMapText)
import Data.Aeson.Lens (AsValue (..), key, members, _String)
import Data.ByteString.Char8 as C (cons, head, lines, null, tail, unlines)
import Data.Char (isAlpha)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict as HM (fromList)
import Data.List (intercalate)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml as Y (ParseException, decodeEither', encode)
import ExMonoid (ExMonoid (..), mBefore, tCreateDir, tReadFile, tRemoveDirWithEmptyParents, tRemoveFile, tWriteFile)
import Filesystem.Path.CurrentOS as Path ()
import Options.Applicative (Alternative ((<|>)), Parser, argument, command, commandGroup, customExecParser, fullDesc, header, help, helper, info, long, metavar, prefs, progDesc, short, showHelpOnError, str, strOption, subparser, value)
import System.Exit (exitFailure)
import System.FilePath (dropTrailingPathSeparator, isValid, pathSeparator, splitDirectories, (<.>), (</>))
import System.FilePath.Posix (takeDirectory)
import System.Process

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

-- Constants

modulesDir :: FilePath
modulesDir = "./Modules"

templatesDir :: FilePath
templatesDir = "./Templates"

packageYaml :: String
packageYaml = "./package.yaml"

managerDir :: FilePath
managerDir = "./manager"

defaultTemplate :: String
defaultTemplate = "Contest"

ghci :: FilePath
ghci = "./.ghci"

-- Types

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

-- Parsers

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

setCommand :: Parser Command
setCommand = CommandSet <$> parseFileName

listCommand :: Parser Command
listCommand = pure CommandList

addCommand :: Parser Command
addCommand = CommandAdd <$> parseFileName <*> parseTemplate

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseFileName

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

-- Helper functions

makeSubCommand :: Target -> Parser Command
makeSubCommand target =
  subparser
    ( f
        "add"
        addCommand
        ( ("Add a" <-> name <-> "at '" <> dir </> "NAME.hs'. It will also appear in the './package.yaml'.")
            <-> "A NAME should be of form 'A(/A)*', like 'A' or 'A/A', where 'A' is an alphanumeric sequence."
            <-> ("A NAME 'A/A' refers to a" <-> name <-> "at" <-> qq (dir </> "A/A.hs"))
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

-- | Select an option depending on the Target constructor
eitherTarget :: p -> p -> Target -> p
eitherTarget f g x =
  case x of
    TargetModule -> f
    TargetTemplate -> g

-- | concatenate strings with a space
(<->) :: (IsString a, Semigroup a) => a -> a -> a
x <-> y = x <> " " <> y

-- | enclose a string into single quotes
qq :: (IsString a, Semigroup a) => a -> a
qq s = "'" <> s <> "'"

updateCabal :: IO ()
updateCabal = do
  putStrLn "Updating .cabal"
  callCommand "hpack"

-- | safely handle command
-- collect into a monoid and rethrow the exceptions that occur when doing or undoing actions
handleCommand :: GeneralCommand -> IO ()
handleCommand (GeneralCommand {..}) = runManaged $ case command_ of
  CommandAdd {name, template} -> do
    throwIfBadName name
    throwIfBadName template
    liftIO $ putStrLn $ "Reading template at" <-> qq templateHs
    t <- tReadFile templateHs (mapThrow ERead TemplateHs templateHs)
    liftIO $ putStrLn $ "Writing the template into" <-> qq fileHs
    tCreateDir targetDir (mapThrow EWrite Directory fileHs) (mapThrow ERemove Directory fileHs)
    tWriteFile fileHs t (mapThrow EWrite FileHs fileHs)
    readPackageYaml $ \y1 atKey nempty -> do
      let y2 =
            y1
              & over
                (key executables . atKey (mkExe name) . nempty . atKey main')
                (\_ -> Just $ String (T.pack fileHs))
      writePackageYaml y2
    liftIO updateCabal
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
    liftIO $ putStrLn $ "Removing" <-> qq fileHs
    tRemoveFile fileHs (mapThrow ERemove FileHs fileHs)
    tRemoveDirWithEmptyParents targetDir (mapThrow ERemove Directory targetDir) (mapThrow EWrite Directory targetDir)
    readPackageYaml $ \y1 _ _ -> do
      let withoutExe x =
            Object $
              KM.fromHashMapText $
                HM.fromList (x ^.. members . withIndex . filtered (\(y, _) -> y /= exe))
          y2 = y1 & over (key executables) withoutExe
      writePackageYaml y2
    liftIO updateCabal
    where
      fileHs = mkTargetHs name
      targetDir = takeDirectory fileHs
      exe = mkExe name
  CommandSet {name} -> do
    throwIfBadName name
    tWriteFile ghci (encodeUtf8 txt) (mapThrow EWrite Ghci ghci)
    where
      txt = T.pack $ ":set -isrc\n:load" <-> mkTargetHs name
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
    throwIfBadName name = mBefore (liftIO $ unless (isOkName name) (throwIO $ NameError name)) id
    mkExe x =
      T.pack $
        eitherTarget "" "Templates." target
          <> ((\y -> if y == pathSeparator then '.' else y) <$> x)
    -- read a package.yaml and run the continuation ok if everything is OK
    readPackageYaml ok = do
      liftIO $ putStrLn $ "Reading" <-> qq packageYaml
      y1 <- tReadFile packageYaml (mapThrow ERead PackageYaml packageYaml)
      let (y2 :: Either ParseException Value) = decodeEither' y1
      case y2 of
        Left l -> liftIO $ throwIO $ FileError ERead PackageYaml packageYaml (show l)
        Right r ->
          let atKey :: Text -> (Maybe Value -> Identity (Maybe Value)) -> Value -> Identity Value
              atKey k = _Object . at k
              nempty :: (Value -> Identity Value) -> Maybe Value -> Identity (Maybe Value)
              nempty = non (Object mempty)
           in ok r atKey nempty
    -- write package.yaml
    writePackageYaml y1 = do
      liftIO $ putStrLn $ "Updating" <-> qq packageYaml
      let y2 =
            C.lines (Y.encode y1)
              <&> (\x -> if not (C.null x) && isAlpha (C.head x) then C.cons '\n' x else x)
              & C.unlines
              & C.tail
      tWriteFile packageYaml y2 (mapThrow EWrite PackageYaml packageYaml)
    -- catch, map an exception into a custom exception for easier handling, throw it
    mapThrow :: ActionType -> PathType -> FilePath -> IO a -> IO a
    mapThrow actionType fileType filePath x =
      x `catch` (\(e :: SomeException) -> throwIO $ FileError actionType fileType filePath (show e))