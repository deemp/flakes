{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Exception (Exception, catch)
import Control.Exception.Base (SomeException, throwIO)
import Control.Lens (At (at), Identity, filtered, non, over, withIndex, (^..), (^?), _2)
import Control.Monad.Except (MonadIO (liftIO), unless)
import Control.Monad.Managed (runManaged)
import Data.Aeson (Value (..), object)
import Data.Aeson.KeyMap as KM (fromHashMapText)
import Data.Aeson.Lens (AsValue (..), key, members, values, _Object, _String)
import Data.ByteString.Char8 as C (cons, head, lines, null, tail, unlines)
import Data.Char (isAlpha, isUpper)
import Data.Foldable (Foldable (fold), traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict as HM (fromList)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml as Y (ParseException, decodeEither', encode)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity (silent)
import ExMonoid (Ex (..), ExMonoid (..), mBefore, tCreateDir, tReadFile, tRemoveDirWithEmptyParents, tWriteFile)
import Filesystem.Path.CurrentOS as Path ()
import Options.Applicative (Parser, argument, command, customExecParser, fullDesc, headerDoc, helper, info, metavar, prefs, showHelpOnError, str, subparser)
import Options.Applicative.Builder (progDescDoc)
import Options.Applicative.Help (Doc, Pretty (pretty), bold, comma, dot, fill, hardline, putDoc, softline, squotes)
import Options.Applicative.Help.Pretty (parens, space, text)
import System.Directory (doesDirectoryExist, doesPathExist)
import System.Exit (exitFailure)
import System.FilePath (dropTrailingPathSeparator, isValid, pathSeparator, splitDirectories, (<.>), (</>))
import System.FilePath.Posix (takeDirectory)
import System.Process (callCommand)

main :: IO ()
main = do
  command' <-
    customExecParser
      (prefs showHelpOnError)
      ( info
          (helper <*> topCommand)
          ( fullDesc
              <> progDescDoc
                ( Just $
                    "Manage repetitive Haskell modules in a" <+> bb "stack" <+> "project"
                )
              <> headerDoc (Just header')
          )
      )

  handleCommand command'
    `catch` ( \e@(ExMonoid _) -> do
                putDoc' (bb "The following problems occured:" <> hardline <> pretty e)
                exitFailure
            )
  putDoc' "Done!"

-- Constants

modulesDir :: FilePath
modulesDir = "Modules"

packageYaml :: FilePath
packageYaml = "package.yaml"

stackYaml :: String
stackYaml = "stack.yaml"

defaultTemplate :: String
defaultTemplate = "Contest"

ghci :: FilePath
ghci = ".ghci"

dotGhcid :: FilePath
dotGhcid = ".ghcid"

ghcid :: FilePath
ghcid = "ghcid"

hieYaml :: FilePath
hieYaml = "hie.yaml"

gitignore :: FilePath
gitignore = ".gitignore"

_FUNCTION_NAME :: String
_FUNCTION_NAME = "<FUNCTION_NAME>"

_EXECUTABLE :: String
_EXECUTABLE = "<EXECUTABLE>"

_EXISTING :: String
_EXISTING = "<EXISTING>"

_NEW :: String
_NEW = "<NEW>"

_LOCATOR :: String
_LOCATOR = "<LOCATOR>"

_LOCATORs :: String
_LOCATORs = _LOCATOR <> "s"

_GHCID_TARGET :: String
_GHCID_TARGET = "<GHCID_TARGET>"

mainHs :: FilePath
mainHs = "Main.hs"

hs :: String
hs = "hs"

manager :: String
manager = "manager"

executable :: String
executable = "executable"

executables :: String
executables = "executables"

main' :: String
main' = "main"

locator :: String
locator = "locator"

locators :: String
locators = "locators"

mkDir :: FilePath -> FilePath
mkDir x = modulesDir </> x

mkModuleHs :: FilePath -> FilePath
mkModuleHs x = mkDir x <.> hs

mkMainHs :: FilePath -> FilePath
mkMainHs x = mkDir x </> mainHs

mkParent :: FilePath -> FilePath
mkParent x = takeDirectory (mkModuleHs x)

module' :: String
module' = "module"

fill' :: Doc
fill' = fill 100 $ text ""

header' :: Doc
header' =
  descriptionBlock
    [ "To simplify references to directories and Haskell modules,",
      bb manager <+> "uses" <+> bb locators <> dot,
      locatorForm,
      fill',
      bb packageYaml <+> "contains an object" <+> bb executables <> dot,
      "Each" <+> bb executable <+> "has a" <+> bb main' <+> "attribute" <> dot,
      "Its value corresponds to this" <+> bb executable <> "'s main module" <> dot,
      "To simplify references to these modules,",
      bb manager <+> "uses" <+> bb locators,
      fold (intersperse (comma <> space) $ bb <$> [_NEW, _EXISTING]) <> dot,
      "Each of these" <+> bb _LOCATORs <+> "refers to an" <+> bb executable,
      "with" <+> bb main' <+> "at" <+> bb (mkMainHs _LOCATOR) <> dot,
      "In" <+> bb packageYaml <> comma <+> "this" <+> bb executable,
      "has a name" <+> bb _EXECUTABLE <> dot,
      bb _EXECUTABLE <+> "is a" <+> bb _LOCATOR,
      "with all" <+> squotes (bb "/") <+> "replaced by" <+> squotes dot <> dot,
      fill',
      "There are other" <+> bb locator <> "s:",
      fold (intersperse (comma <> space) $ bb <$> [_GHCID_TARGET]) <> dot,
      "Each of these" <+> bb _LOCATORs <+> "refers to a" <+> bb module',
      "at" <+> bb (mkModuleHs _LOCATOR),
      fill',
      "Run" <+> bb "manager COMMAND --help",
      "to learn more about a" <+> bb "COMMAND"
    ]

-- Types
data Command
  = CommandList
  | CommandInit
  | CommandUpdate
  | CommandAdd
      { name :: String,
        template :: String
      }
  | CommandRemove
      { name :: String
      }
  | CommandSet
      { name :: String,
        function :: String
      }
  deriving (Show)

newtype GeneralCommand = GeneralCommand
  { command_ :: Command
  }
  deriving (Show)

data PathType = FileHs | TemplateHs | PackageYaml | Ghci | Ghcid | Directory

instance Show PathType where
  show :: PathType -> String
  show = \case
    FileHs -> "file"
    TemplateHs -> "template"
    PackageYaml -> packageYaml
    Ghci -> ghci
    Directory -> "directory"
    Ghcid -> dotGhcid

instance Pretty PathType where
  pretty :: PathType -> Doc
  pretty = text . show

data ActionType = ERead | EWrite | ERemove

instance Show ActionType where
  show :: ActionType -> String
  show = \case
    ERead -> "reading"
    EWrite -> "writing"
    ERemove -> "removing"

instance Pretty ActionType where
  pretty :: ActionType -> Doc
  pretty = text . show

data ProcessError
  = FileError
      { actionType :: ActionType,
        fileType :: PathType,
        filePath :: FilePath,
        message :: String
      }
  | NameError {name :: String}

descriptionBlock :: [Doc] -> Doc
descriptionBlock desc = fold (intersperse softline desc) <> hardline

descriptionBlock' :: [Doc] -> Doc
descriptionBlock' desc = fold (intersperse softline desc)

(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> softline <> y

instance Pretty ProcessError where
  pretty :: ProcessError -> Doc
  pretty FileError {..} = "Error" <+> pretty actionType <+> pretty fileType <+> pretty filePath <+> ":" <+> pretty message
  pretty NameError {name} =
    descriptionBlock
      [ "Error: the" <+> bb _LOCATOR <+> "of the form" <+> bb name <+> "is bad." <> locatorForm
      ]

locatorForm :: Doc
locatorForm =
  descriptionBlock'
    [ "Each" <+> bb locator <+> "called" <+> bb _LOCATOR,
      "should be of the form" <+> bb "A(/A)*",
      "like" <+> bb "A" <+> "or" <+> bb "A/A",
      "where" <+> bb "A" <+> "is an alphanumeric sequence",
      "starting with an uppercase letter" <> dot
    ]

instance Show ProcessError where
  show :: ProcessError -> String
  show = show

instance Exception ProcessError

-- Parsers

topCommand :: Parser Command
topCommand = makeCommand

initCommand :: Parser Command
initCommand = pure CommandInit

updateCommand :: Parser Command
updateCommand = pure CommandUpdate

parseStringMetavar :: IsString a => String -> Parser a
parseStringMetavar x = argument str (metavar x)

setCommand :: Parser Command
setCommand = CommandSet <$> parseStringMetavar _GHCID_TARGET <*> parseStringMetavar _FUNCTION_NAME

listCommand :: Parser Command
listCommand = pure CommandList

addCommand :: Parser Command
addCommand = CommandAdd <$> parseStringMetavar _NEW <*> parseStringMetavar _EXISTING

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseStringMetavar _EXISTING

-- Helper functions

makeCommand :: Parser Command
makeCommand =
  subparser $
    f
      "add"
      addCommand
      [ "Create an" <+> bb executable <+> "with a locator" <+> bb _NEW,
        "from an existing" <+> bb executable <+> "with a locator" <+> bb _EXISTING,
        "Other modules can be created in" <+> bb (modulesDir </> _NEW),
        "and, e.g., be imported into" <+> bb (modulesDir </> _NEW </> mainHs)
      ]
      <> f
        "rm"
        removeCommand
        [ "Remove an executable with a" <+> bb locator <+> bb _EXISTING,
          "and its empty parent directories"
        ]
      <> f
        "list"
        listCommand
        [ "List" <+> bb "executables" <+> "and their" <+> bb "source-dirs" <> dot,
          "These are also available in" <+> bb packageYaml <> dot
        ]
      <> f
        "set"
        setCommand
        [ "Set" <+> bb dotGhcid <+> "config",
          "so that when you run" <+> bb ghcid,
          "in current directory,",
          bb ghcid <+> "will restart",
          "the function" <+> bb _FUNCTION_NAME <+> "from" <+> bb ghcidTargetHs,
          "when any files change in" <+> bb modulesDir
        ]
      <> f
        "init"
        initCommand
        [ "Initialize a" <+> bb manager <+> "project",
          "in current directory.",
          "Also, initialize a git repository.",
          "This action will remove all user files",
          "except for" <+> bb ".git",
          "(if they exist)",
          "in current directory."
        ]
      <> f
        "update"
        updateCommand
        [ "Generate again" <+> bb ".cabal" <+> "and" <+> bb hieYaml <> dot,
          bb manager <+> "commands update these files automatically.",
          "Run this command whenever you" <+> bb "manually",
          "add or remove a Haskell module."
        ]
  where
    targetDir = modulesDir </> _GHCID_TARGET
    ghcidTargetHs = targetDir <.> hs
    f name' command' desc =
      command
        name'
        ( info
            (helper <*> command')
            ( fullDesc
                <> progDescDoc
                  ( Just $ descriptionBlock desc
                  )
            )
        )

-- | concatenate strings with a space
(<->) :: (IsString a, Semigroup a) => a -> a -> a
x <-> y = x <> " " <> y

bb :: String -> Doc
bb s = bold $ text s

updateHsProjectFiles :: IO ()
updateHsProjectFiles = do
  putDoc' ("Updating" <+> bb ".cabal" <> hardline)
  callCommand "hpack"
  putDoc' ("Updating" <+> bb "hie.yaml" <> hardline)
  callCommand "gen-hie --stack > hie.yaml"

-- copy all from template
-- exclude stack yaml and stack lock

nixFlakeInit :: IO ()
nixFlakeInit = do
  let cleanCurrentDirectory = "rm -rf ..?* .[!.][!git]* *"
      initCodiumHaskell = "nix flake init -t github:deemp/flakes/main#codium-haskell"
      initManager = "nix flake init -t github:deemp/flakes/main?dir=manager#init"
      removeConflicts =
        "rm -rf"
          <-> unwords
            [ "*.cabal",
              "src",
              "app",
              "test",
              "README.md",
              packageYaml,
              modulesDir,
              hieYaml,
              gitignore
            ]
      gitInit = "git init"
      gitCommit = "git add . && git commit -m 'manager init'"
  putDoc' ("Cleaning current directory:" <+> bb cleanCurrentDirectory)
  callCommand cleanCurrentDirectory
  putDoc' ("Writing a template" <+> bb "VSCodium for Haskell" <> ":" <+> bb initCodiumHaskell)
  callCommand initCodiumHaskell
  putDoc' ("Removing conflicting files:" <+> bb removeConflicts)
  callCommand removeConflicts
  putDoc' ("Writing a template" <+> bb "manager" <> ":" <+> bb initManager)
  callCommand initManager
  putDoc' "Checking for an existing git repository in current directory..."
  isGitInitialized <- doesDirectoryExist ".git"
  unless isGitInitialized $ do
    putDoc' ("Initializing a" <+> bb "git" <+> "repository:" <+> bb gitInit)
    callCommand gitInit
  updateHsProjectFiles
  putDoc' ("Committing new changes:" <+> bb gitCommit)
  callCommand gitCommit

putDoc' :: Doc -> IO ()
putDoc' x = putDoc (x <> hardline)

-- TODO take templates from files, not from path/Main
-- TODO manager set [--module|-m] module [--function|-f] fun

throwEx :: Doc -> IO a
throwEx x = throwIO $ Ex x

-- | safely handle command
-- collect into a monoid and rethrow the exceptions that occur when doing or undoing actions
handleCommand :: Command -> IO ()
handleCommand cmd = runManaged $ case cmd of
  CommandInit -> liftIO nixFlakeInit
  CommandUpdate -> liftIO updateHsProjectFiles
  CommandAdd {name, template} -> do
    throwIfBadName name
    throwIfBadName template
    readPackageYaml $ \y1 atKey nempty -> do
      let templateExe = mkExe template
          atTemplate = y1 ^? key executables' . key templateExe
      liftIO $ unless (isJust atTemplate) (throwEx $ "No such executable" <+> bb (T.unpack templateExe) <+> "in" <+> bb packageYaml)
      let atExe k = key executables' . atKey (mkExe k) . nempty
          atExeKey k v =
            over
              (key executables' . atKey (mkExe name) . nempty . atKey k)
              (\_ -> Just $ String (T.pack v))
          y2 =
            y1
              & over (atExe name) (\_ -> fromMaybe (object mempty) atTemplate)
              & atExeKey (T.pack main') mainHs
              & atExeKey sourceDirs targetDir
      writePackageYaml y2
    liftIO $ putDoc' $ "Creating" <+> bb targetDir
    tCreateDir targetDir (mapThrow EWrite Directory targetDir) (mapThrow ERemove Directory targetDir)
    liftIO $ putDoc' $ "Copying files from" <+> bb fromDir <+> "into" <+> bb targetDir
    ex <- liftIO $ doesPathExist fromDir
    liftIO $ unless ex (throwEx $ bb fromDir <+> "does not exist")
    liftIO $ copyDirectoryRecursive silent fromDir targetDir
    liftIO updateHsProjectFiles
    where
      targetDir = mkDir name
      fromDir = mkDir template
  CommandList -> readPackageYaml $ \y1 _ _ -> do
    liftIO $ putDoc' "Executable ( source-dirs ):"
    let y2 =
          y1 ^.. key executables' . members . withIndex
            <&> over
              _2
              ( \y ->
                  T.intercalate
                    ", "
                    ( y ^.. key sourceDirs . values . _String
                        <> y ^.. key sourceDirs . _String
                    )
              )
    traverse_ (\(name, path) -> liftIO $ putDoc' $ text (T.unpack name) <+> parens ("" <+> text (T.unpack path) <+> "")) y2
  CommandRemove {name} -> do
    throwIfBadName name
    ex <- liftIO $ doesDirectoryExist targetDir
    liftIO $ unless ex $ throwEx $ bb targetDir <+> "doesn't exist"
    liftIO $ putDoc' $ "Removing" <+> bb targetDir
    tRemoveDirWithEmptyParents targetDir (mapThrow ERemove Directory targetDir) (mapThrow EWrite Directory targetDir)
    readPackageYaml $ \y1 _ _ -> do
      let withoutExe x =
            Object $
              KM.fromHashMapText $
                HM.fromList (x ^.. members . withIndex . filtered (\(y, _) -> y /= mkExe name))
          y2 = y1 & over (key executables') withoutExe
      writePackageYaml y2
    liftIO updateHsProjectFiles
    where
      targetDir = mkDir name
  CommandSet {name, function} -> do
    throwIfBadName name
    tWriteFile dotGhcid (encodeUtf8 txtGhcid) (mapThrow EWrite Ghci dotGhcid)
    where
      txtGhcid =
        T.pack $
          intercalate
            "\n"
            [ "-c stack ghci" <-> mkModuleHs name,
              "-W",
              "-r=" <> function,
              "--reload=" <> modulesDir
            ]
  where
    sourceDirs = "source-dirs"
    executables' = T.pack executables
    isOkName name =
      isValid name
        && hasNoTrailingSeparator
        && all
          (\x -> all (`elem` alphabet) x && isUpper (Prelude.head x))
          (splitDirectories name)
      where
        hasNoTrailingSeparator = dropTrailingPathSeparator name == name
        alphabet = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']
    throwIfBadName name = mBefore (liftIO $ unless (isOkName name) (throwIO $ NameError name)) id
    replace cond rep s = (\x -> if cond x then rep else x) <$> s
    mkExe x = T.pack $ replace (== pathSeparator) '.' x
    -- read a package.yaml and run the continuation ok if everything is OK
    readPackageYaml ok = do
      liftIO $ putDoc' $ "Reading" <-> bb packageYaml
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
      liftIO $ putDoc' $ "Updating" <-> bb packageYaml
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
