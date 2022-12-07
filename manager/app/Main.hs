{-# LANGUAGE ImportQualifiedPost #-}
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
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml as Y (ParseException, decodeEither', encode)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity (silent)
import ExMonoid (Ex (..), ExMonoid (..), mBefore, tCreateDir, tReadFile, tRemoveDirWithEmptyParents, tWriteFile)
import Filesystem.Path.CurrentOS as Path ()
import Options.Applicative (Parser, action, argument, command, customExecParser, fullDesc, headerDoc, helper, info, metavar, prefs, showHelpOnError, str, subparser)
import Options.Applicative.Builder (progDescDoc)
import Options.Applicative.Help (Doc, Pretty (pretty), bold, comma, dot, fill, hardline, putDoc, softline, squotes)
import Options.Applicative.Help.Pretty (parens, space, text)
import System.Directory (doesDirectoryExist, doesPathExist)
import System.Exit (exitFailure)
import System.FilePath (dropTrailingPathSeparator, isValid, splitDirectories, (<.>), (</>))
import System.FilePath qualified as FP (hasExtension)
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

srcDir :: FilePath
srcDir = "src"

packageYaml :: FilePath
packageYaml = "package.yaml"

stackYaml :: String
stackYaml = "stack.yaml"

ghci :: FilePath
ghci = ".ghci"

ghcid :: FilePath
ghcid = "ghcid"

dotGhcid :: FilePath
dotGhcid = ".ghcid"

hieYaml :: FilePath
hieYaml = "hie.yaml"

gitignore :: FilePath
gitignore = ".gitignore"

_FUNCTION_NAME :: String
_FUNCTION_NAME = "<FUNCTION_NAME>"

_EXECUTABLE :: String
_EXECUTABLE = "<EXECUTABLE>"

_PATH :: String
_PATH = "path"

_EXECUTABLE_PATH :: String
_EXECUTABLE_PATH = "<EXECUTABLE_DIRECTORY>"

_EXISTING :: String
_EXISTING = "<EXISTING_DIRECTORY>"

_NEW :: String
_NEW = "<NEW_DIRECTORY>"

_GHCID_TARGET :: String
_GHCID_TARGET = "<EXISTING_MODULE>"

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

metavariable :: String
metavariable = "metavariable"

metavariables :: String
metavariables = "metavariables"

locators :: String
locators = "locators"

mkDir :: FilePath -> FilePath
mkDir x = x

mkModuleHs :: FilePath -> FilePath
mkModuleHs x = x <.> hs

mkMainHs :: FilePath -> FilePath
mkMainHs x = mkDir x </> mainHs

mkParent :: FilePath -> FilePath
mkParent x = takeDirectory (mkModuleHs x)

module' :: String
module' = "module"

dotCabal :: FilePath
dotCabal = ".cabal"

path' :: Doc
path' = bb "path"

paths' :: Doc
paths' = bb "paths"

fill' :: Doc
fill' = fill 100 $ text ""

listVariables :: [String] -> Doc
listVariables vs = fold (intersperse (comma <> space) $ bb <$> vs)

header' :: Doc
header' =
  descriptionBlock
    [ bb manager <+> "simplifies routine actions"
    , "in projects with multiple" <+> bb mainHs <+> "modules."
    , "Run" <+> bb "manager COMMAND --help"
    , "to learn more about a" <+> bb "COMMAND" <> dot
    , fill'
    , "Let's explore one of its commands by typing"
    , bb (manager <> " " <> "add") <> dot
    , "Then, you will see the" <+> bb metavariables
    , bb _NEW <+> "and" <+> bb _EXISTING <> dot
    , "When writing" <+> bb manager <+> "commands,"
    , "you should provide paths" <+> "to Haskell modules" <+> "or to directories"
    , "instead of these" <+> bb metavariables <> dot
    , fill'
    , "Next," <+> bb manager <+> "simplifies" <+> bb ghcid <+> "configuration."
    , "This configuration is stored in a" <+> bb dotGhcid <+> "file."
    , "Type" <+> bb "manager set" <+> "and read its help message."
    , "The" <+> bb _GHCID_TARGET <+> "refers to a Haskell module"
    , "that contains a" <+> bb _FUNCTION_NAME <> dot
    , fill'
    , "Furthermore, some executables provided by this flake," <+> "e.g.," <+> bb "stack" <> comma
    , "require" <+> bb executable <+> "names."
    , "These names come from" <+> bb packageYaml <> dot
    , "The" <+> bb packageYaml <+> "contains an object" <+> bb executables <> dot
    , "Each" <+> bb executable <+> "there has a" <+> bb main' <+> "attribute" <> dot
    , "Its value is the name of this" <+> bb executable <> "'s main module" <+> parens (bb mainHs) <> dot
    , "To refer to an" <+> bb executable <+> "in commands,"
    , bb manager <+> "uses a path to the parent directory of this" <+> bb executable <> "'s" <+> bb mainHs <> dot
    , "Let's call such a path an" <+> bb _EXECUTABLE_PATH <> dot
    , "The" <+> bb metavariables <+> "that refer to an" <+> bb _EXECUTABLE_PATH <+> "are"
    , fold (intersperse (comma <> space) $ bb <$> [_NEW, _EXISTING]) <> dot
    , "An" <+> bb _EXECUTABLE <+> "is an" <+> bb _EXECUTABLE_PATH <> comma
    , "with prefix" <+> bb srcDir <+> "removed"
    , "and with all" <+> squotes (bb "/") <+> "replaced by" <+> squotes dot <> dot
    ]

-- Types
data Command
  = CommandList
  | CommandInit
  | CommandUpdate
  | CommandAdd
      { newDir :: FilePath
      , existingDir :: String
      }
  | CommandRemove
      { existingDir :: FilePath
      }
  | CommandSet
      { existingModule :: FilePath
      , function :: String
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
    PackageYaml -> packageYaml <> " " <> _PATH
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
      { actionType :: ActionType
      , fileType :: PathType
      , filePath :: FilePath
      , message :: String
      }
  | NameError
      { pathType :: ManagerPathType
      , name :: String
      }

descriptionBlock :: [Doc] -> Doc
descriptionBlock desc = fold (intersperse softline desc) <> hardline

descriptionBlock' :: [Doc] -> Doc
descriptionBlock' desc = fold (intersperse softline desc)

(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> softline <> y

instance Pretty ProcessError where
  pretty :: ProcessError -> Doc
  pretty FileError{..} = "Error" <+> pretty actionType <+> pretty fileType <+> pretty filePath <+> ":" <+> pretty message
  pretty NameError{..} =
    descriptionBlock
      [ "Error: the" <+> bb _PATH <+> "of the form" <+> bb name <+> "is bad." <> pathForm pathType
      ]

data ManagerPathType = MDirectory | MModuleHs

eitherManagerPath :: p -> p -> ManagerPathType -> p
eitherManagerPath f g p =
  case p of
    MDirectory -> f
    MModuleHs -> g

instance Show ManagerPathType where
  show :: ManagerPathType -> String
  show MDirectory = "directory"
  show MModuleHs = "module"

pathForm :: ManagerPathType -> Doc
pathForm m =
  descriptionBlock'
    [ "Each" <> text (show m) <> "path must be of the form"
    , bb (srcDir <> "/<A>(/<A>)*" <> eitherManagerPath "" ".hs" m)
    , "like" <+> bb (srcDir <> "/<A>") <+> "or" <+> bb (srcDir <> "/<A>/<A>")
    , "where" <+> bb "<A>" <+> "is an alphanumeric sequence"
    , "starting with an uppercase letter" <> dot
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

addCommand :: Parser Command
addCommand = CommandAdd <$> parseStringMetavar _NEW <*> parseStringMetavar _EXISTING

setCommand :: Parser Command
setCommand = CommandSet <$> parseStringMetavar _GHCID_TARGET <*> parseStringMetavar _FUNCTION_NAME

listCommand :: Parser Command
listCommand = pure CommandList

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseStringMetavar _EXISTING

-- Helper functions

makeCommand :: Parser Command
makeCommand =
  subparser $
    f
      "add"
      addCommand
      [ "Create an" <+> bb executable <+> "with a path" <+> bb _NEW
      , "from an existing" <+> bb executable <+> "with a path" <+> bb _EXISTING <> dot
      , "In the" <+> bb _NEW
      , "you can create new Haskell modules"
      , "and import them as usually into other modules in" <+> bb _NEW
      ]
      <> f
        "rm"
        removeCommand
        [ "Remove an executable with a" <+> path' <+> bb _EXISTING
        , "and its empty parent directories."
        ]
      <> f
        "list"
        listCommand
        [ "List" <+> bb "executables" <+> "and their" <+> bb "source-dirs" <> dot
        , "These are also available in" <+> bb packageYaml <> dot
        ]
      <> f
        "set"
        setCommand
        [ "Set" <+> bb dotGhcid <+> "config"
        , "so that when you run" <+> bb ghcid <+> "in the current directory,"
        , bb ghcid <+> "will restart"
        , "the function" <+> bb _FUNCTION_NAME <+> "from" <+> bb _GHCID_TARGET
        , "module whenever any files change in" <+> bb srcDir <> dot
        , bb ghcid <+> "won't restart if such function creates a new process"
        , parens "e.g., a server" <> dot
        , "So, in case you'd like to restart, you'd better use" <+> bb ghci
        ]
      <> f
        "init"
        initCommand
        [ "Initialize a" <+> bb manager <+> "project in the current directory."
        , "Also, initialize a" <+> bb "git repository" <+> "if not already present."
        , "Remove all user files" <+> "in the current directory" <+> "and commit changes"
        ]
      <> f
        "update"
        updateCommand
        [ "Generate again" <+> bb dotCabal <+> "and" <+> bb hieYaml <> dot
        , bb manager <+> "commands update these files automatically."
        , "Run this command whenever you" <+> bb "manually"
        , "add or remove a Haskell module."
        ]
 where
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
            [ "*.cabal"
            , "src"
            , "app"
            , "test"
            , "README.md"
            , packageYaml
            , srcDir
            , hieYaml
            , gitignore
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

throwEx :: Doc -> IO a
throwEx x = throwIO $ Ex x

mkExe :: FilePath -> T.Text
mkExe path = T.pack $ intercalate "." (withoutSrc (splitDirectories (withoutFile path)))

{-
>>>mkDirectory <$> testNames
[Nothing,Nothing,Nothing,Just "A.B",Just "A.B",Nothing,Nothing]
-}

testNames :: [String]
testNames = ["a/b", "src/a/b", "src/a/b/c.hs", "src/A/B/c.hs", "src/A/B/C.hs", "A/B.hs", "src/A.hs"]

withoutFile :: FilePath -> FilePath
withoutFile path
  | FP.hasExtension path = takeDirectory path
  | otherwise = path

withoutSrc :: [a] -> [a]
withoutSrc = Prelude.tail

isOkPath :: FilePath -> Bool
isOkPath path =
  isValid path
    && hasSrc
    && hasCorrectParts
    && hasNoTrailingSeparator
 where
  withoutFile' = withoutFile path
  parts = splitDirectories withoutFile'
  hasSrc =
    length (splitDirectories withoutFile') > 1
      && take 1 (splitDirectories withoutFile') == [srcDir]
  alphabet = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']
  hasCorrectParts =
    all
      (\x -> all (`elem` alphabet) x && isUpper (Prelude.head x))
      (withoutSrc parts)
  hasNoTrailingSeparator = dropTrailingPathSeparator path == path

{-
>>>takeExtension <$> ["a/b", "a/c.hs"]
["",".hs"]
>>>takeBaseName <$> ["a/b", "a/c.hs"]
["b","c"]
>>>isOkPath <$> testNames
[False,False,False,True,True]
-}

{- | safely handle command
 collect into a monoid and rethrow the exceptions that occur when doing or undoing actions
-}
handleCommand :: Command -> IO ()
handleCommand cmd = do
  runManaged $ case cmd of
    CommandInit -> liftIO nixFlakeInit
    CommandUpdate -> liftIO updateHsProjectFiles
    CommandAdd{newDir, existingDir} -> do
      throwIfBadName MDirectory newDir
      throwIfBadName MDirectory existingDir
      readPackageYaml $ \y1 atKey nempty -> do
        let existingExe = mkExe existingDir
            atExisting = y1 ^? key executables' . key existingExe
        liftIO $ unless (isJust atExisting) (throwEx $ "No such executable" <+> bb (T.unpack existingExe) <+> "in" <+> bb packageYaml)
        let atExe k = key executables' . atKey (mkExe k) . nempty
            atExeKey k v =
              over
                (key executables' . atKey (mkExe newDir) . nempty . atKey k)
                (\_ -> Just $ String (T.pack v))
            y2 =
              y1
                & over (atExe newDir) (\_ -> fromMaybe (object mempty) atExisting)
                & atExeKey (T.pack main') mainHs
                & atExeKey sourceDirs newDir
        writePackageYaml y2
      liftIO $ putDoc' $ "Creating" <+> bb newDir
      tCreateDir newDir (mapThrow EWrite Directory newDir) (mapThrow ERemove Directory newDir)
      liftIO $ putDoc' $ "Copying files from" <+> bb existingDir <+> "into" <+> bb newDir
      ex <- liftIO $ doesPathExist existingDir
      liftIO $ unless ex (throwEx $ bb existingDir <+> "does not exist")
      liftIO $ copyDirectoryRecursive silent existingDir newDir
      liftIO updateHsProjectFiles
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
    CommandRemove{existingDir} -> do
      throwIfBadName MDirectory existingDir
      throwIfNoSuchDir existingDir
      liftIO $ putDoc' $ "Removing" <+> bb existingDir
      tRemoveDirWithEmptyParents existingDir (mapThrow ERemove Directory existingDir) (mapThrow EWrite Directory existingDir)
      readPackageYaml $ \y1 _ _ -> do
        let withoutExe x =
              Object $
                KM.fromHashMapText $
                  HM.fromList (x ^.. members . withIndex . filtered (\(y, _) -> y /= mkExe existingDir))
            y2 = y1 & over (key executables') withoutExe
        writePackageYaml y2
      liftIO updateHsProjectFiles
    CommandSet{existingModule, function} -> do
      throwIfBadName MModuleHs existingModule
      tWriteFile dotGhcid (encodeUtf8 txtGhcid) (mapThrow EWrite Ghci dotGhcid)
     where
      txtGhcid =
        T.pack $
          intercalate
            "\n"
            [ "-c stack ghci" <-> mkModuleHs existingModule
            , "-W"
            , "-r=" <> function
            , "--reload=" <> srcDir
            ]
 where
  sourceDirs = "source-dirs"
  executables' = T.pack executables
  throwIfBadName pathType path = liftIO $ unless (isOkPath path) (throwEx $ text $ show $ NameError pathType path)
  throwIfNoSuchDir path =
    mBefore
      ( liftIO $ do
          ex <- doesDirectoryExist path
          unless ex (throwIO $ FileError ERead Directory path "no such directory")
      )
      id
  -- doesDirectoryExist name >>= \x -> when x
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
