{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Exception (Exception, catch)
import Control.Exception.Base (SomeException, throwIO)
import Control.Lens (At (at), Identity, filtered, non, over, withIndex, (^..), (^?), _2)
import Control.Monad (when)
import Control.Monad.Except (MonadIO (liftIO), unless)
import Control.Monad.Managed (runManaged)
import Data.Aeson (Value (..), object)
import Data.Aeson.KeyMap as KM (fromHashMapText)
import Data.Aeson.Lens (AsValue (..), key, members, values, _Object, _String)
import Data.ByteString.Char8 as C (cons, head, lines, null, tail, unlines)
import Data.Char (isAlpha, isUpper)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict as HM (fromList)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml as Y (ParseException, decodeEither', encode)
import ExMonoid (Ex (..), ExMonoid (..), mBefore, tCreateDir, tReadFile, tRemoveDirWithEmptyParents, tRemoveFile, tWriteFile)
import Filesystem.Path.CurrentOS as Path ()
import Inits (initGitIgnore, initPackageYaml, initSimpleMain, initStackYaml)
import Options.Applicative (Alternative ((<|>)), Parser, argument, command, commandGroup, customExecParser, fullDesc, headerDoc, helper, info, metavar, prefs, progDesc, showHelpOnError, str, subparser)
import Options.Applicative.Builder (progDescDoc)
import Options.Applicative.Help (Doc, Pretty (pretty), bold, comma, dot, hardline, putDoc, softline, (<+>))
import Options.Applicative.Help.Pretty (parens, text)
import System.Directory (doesDirectoryExist, doesFileExist)
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
          (helper <*> generalCommand)
          ( fullDesc
              <> progDescDoc
                ( Just $
                    "Organize Haskell modules and template files"
                      <> (softline <> "in a" <+> bb "stack" <+> "project so that" <+> bb "HLS" <+> "accepts them")
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
modulesDir = "./Modules"

packageYaml :: FilePath
packageYaml = "./package.yaml"

stackYaml :: String
stackYaml = "./stack.yaml"

defaultTemplate :: String
defaultTemplate = "Contest"

ghci :: FilePath
ghci = "./.ghci"

ghcid :: FilePath
ghcid = "./.ghcid"

hieYaml :: FilePath
hieYaml = "./hie.yaml"

gitignore :: FilePath
gitignore = "./.gitignore"

_NAME :: String
_NAME = "NAME"

_TEMPLATE_NAME :: String
_TEMPLATE_NAME = "TEMPLATE_NAME"

mainHs :: FilePath
mainHs = "Main.hs"

manager :: String
manager = "manager"

header' :: Doc
header' =
  (bb _NAME <+> "should be of form" <+> bb "A(/A)*" <+> "like" <+> bb "A" <+> "or" <+> bb "A/A")
    <> (softline <> "where" <+> bb "A" <+> "is an alphanumeric sequence starting with an uppercase letter" <> dot)
    <> (softline <> bb _NAME <+> "refers to a module at" <+> bb (modulesDir </> _NAME </> mainHs) <> dot)
    <> (softline <> bb _TEMPLATE_NAME <+> "should be of the same form as" <+> bb _NAME <> dot)
    <> (softline <> bb _TEMPLATE_NAME <+> "refers to a template at" <+> bb (modulesDir </> _TEMPLATE_NAME <.> "hs") <> dot)

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
      { name :: String
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
    Ghcid -> ghcid

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

instance Pretty ProcessError where
  pretty :: ProcessError -> Doc
  pretty FileError {..} = "Error" <+> pretty actionType <+> pretty fileType <+> pretty filePath <+> ":" <+> pretty message
  pretty NameError {name} =
    ("Error: the" <+> bb _NAME <+> "or" <+> bb _TEMPLATE_NAME <+> "of the form" <+> bb name <+> "is bad.")
      <> (softline <> "It should be of form" <+> bb "A(/A)*" <+> "like" <+> bb "A" <+> "or" <+> bb "A/A" <> comma)
      <> (softline <> "where" <+> bb "A" <+> "is an alphanumeric sequence starting with an uppercase letter" <> dot <> hardline)

instance Show ProcessError where
  show :: ProcessError -> String
  show = show

instance Exception ProcessError

-- Parsers

modulesSubCommand :: Parser Command
modulesSubCommand = makeSubCommand

generalCommand :: Parser GeneralCommand
generalCommand = GeneralCommand <$> modulesSubCommand

initCommand :: Parser Command
initCommand = pure CommandInit

updateCommand :: Parser Command
updateCommand = pure CommandUpdate

setCommand :: Parser Command
setCommand = CommandSet <$> parseModule

listCommand :: Parser Command
listCommand = pure CommandList

addCommand :: Parser Command
addCommand = CommandAdd <$> parseModule <*> parseTemplate

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseModule

parseModule :: Parser String
parseModule = argument str (metavar _NAME)

parseTemplate :: Parser String
parseTemplate = argument str (metavar _TEMPLATE_NAME)

-- Helper functions

val :: Value
val = object [("executables", object [("Book", object [("deps", "vals")])])]

-- f :: Value
ff :: Value
ff = fromMaybe (object []) (val ^? key "executables" . key "Book")

-- _Object

-- s = val ^? key "a"

{-
>>> f
Object (fromList [])
-}

makeSubCommand :: Parser Command
makeSubCommand =
  subparser
    ( f
        "add"
        addCommand
        ( ("Create a module at" <+> bbModulePath)
            <> (softline <> "from a template at" <> bbTemplatePath <> dot)
            <> (softline <> "It will also appear in the" <> bb packageYaml <> dot)
            <> (softline <> "You may create other modules at" <+> bb moduleDir <> softline <> "and import them into" <+> bbModulePath <> dot)
            <> hardline
        )
        <> f
          "rm"
          removeCommand
          ( ("Remove the directory of" <+> bbModulePath <> softline <> "and its empty parent directories" <> dot)
              <> (softline <> "This" <+> name <+> "will also be removed from" <+> bb packageYaml <> dot)
              <> hardline
          )
        <> f
          "list"
          listCommand
          ( ("List modules'" <+> bb "executables" <+> "and their" <+> bb "source-dirs" <> dot)
              <> (softline <> "These are also available in" <+> bb packageYaml <> dot)
              <> hardline
          )
        <> f
          "set"
          setCommand
          ( ("Set the" <+> name <+> bbModulePath)
              <> (softline <> "so that it's loaded when a" <+> bb "ghci" <+> "session starts" <> dot)
              <> (softline <> "When" <+> bb "ghcid" <+> "starts")
              <> (softline <> "it will run this " <> name <> "'s" <+> bb "main" <+> "function" <> dot)
              <> hardline
          )
        <> f
          "init"
          initCommand
          ( ("Initialize a managed" <+> bb "stack" <+> "project")
              <> (softline <> "in current directory")
              <> (softline <> "This action will remove all files in current directory")
              <> (softline <> "and it cannot be undone")
              <> hardline
          )
        <> f
          "update"
          updateCommand
          ( ("Re-generate" <+> bb ".cabal" <+> "and" <+> bb "hie.yaml" <> dot)
              <> (softline <> "Run whenever you manyally add a module" <> dot)
              <> (softline <> bb manager <+> "commands update these files automatically" <> dot)
              <> hardline
          )
    )
  where
    moduleDir = modulesDir </> _NAME
    templatePath = modulesDir </> _TEMPLATE_NAME
    bbModulePath = bb (moduleDir </> "Main.hs")
    bbTemplatePath = bb (templatePath <.> "hs")
    f name' command' desc = command name' (info (helper <*> command') (fullDesc <> progDescDoc (Just desc)))
    name = "module"

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
  let cleanCurrentDirectory = "rm -rf ./*"
      initCodiumHaskell = "nix flake init -t github:br4ch1st0chr0n3/flakes#codium-haskell"
      initManager = "nix flake init -t github:br4ch1st0chr0n3/flakes?dir=manager#init"
      removeConflicts = "rm -rf" <-> unwords [packageYaml, "*.cabal", "src", "app", "test", "Modules", hieYaml]
      gitInit = "git init"
      gitCommit = "git add . && git commit -m 'manager init'"
  putDoc' ("Cleaning current directory:" <+> bb cleanCurrentDirectory)
  callCommand cleanCurrentDirectory
  putDoc' ("Writing a template" <+> bb "VSCodium for Haskell" <> ":" <+> bb initCodiumHaskell)
  callCommand initCodiumHaskell
  putDoc' ("Removing conflicting files:" <+> bb removeConflicts)
  callCommand initCodiumHaskell
  putDoc' ("Writing a template" <+> bb "manager" <> ":" <+> bb initManager)
  callCommand initManager
  isGitInitialized <- doesDirectoryExist ".git"
  unless isGitInitialized $ do
    putDoc' ("Initializing a" <+> bb "git" <+> "repository:" <+> bb gitInit)
    callCommand gitInit
  putDoc' ("Committing new changes:" <+> bb gitCommit)
  callCommand gitCommit

putDoc' :: Doc -> IO ()
putDoc' x = putDoc (x <> hardline)

-- TODO take templates from files, not from path/Main
-- TODO manager set [--module|-m] module [--function|-f] fun

-- | safely handle command
-- collect into a monoid and rethrow the exceptions that occur when doing or undoing actions
handleCommand :: GeneralCommand -> IO ()
handleCommand (GeneralCommand {..}) = runManaged $ case command_ of
  CommandInit -> do
    liftIO nixFlakeInit
    liftIO updateHsProjectFiles
  CommandUpdate -> liftIO updateHsProjectFiles
  CommandAdd {name, template} -> do
    throwIfBadName name
    throwIfBadName template
    readPackageYaml $ \y1 atKey nempty -> do
      let templateExe = mkExe template
          atTemplate = y1 ^? key executables . key templateExe
      liftIO $ unless (isJust atTemplate) (throwIO $ Ex $ "No such executable" <+> bb (T.unpack templateExe) <+> "in" <+> bb packageYaml)
      let atExe k = key executables . atKey (mkExe k) . nempty
          atExeKey k v =
            over
              (key executables . atKey (mkExe name) . nempty . atKey k)
              (\_ -> Just $ String (T.pack v))
          y2 =
            y1
              & over (atExe name) (\_ -> fromMaybe (object mempty) atTemplate)
              & atExeKey main' mainHs
              & atExeKey sourceDirs targetDir
      writePackageYaml y2
    liftIO $ putDoc' $ "Reading template at" <+> bb templateHs
    t <- tReadFile templateHs (mapThrow ERead TemplateHs templateHs)
    liftIO $ putDoc' $ "Writing the template into" <+> bb fileHs
    tCreateDir targetDir (mapThrow EWrite Directory fileHs) (mapThrow ERemove Directory fileHs)
    tWriteFile fileHs t (mapThrow EWrite FileHs fileHs)
    liftIO updateHsProjectFiles
    where
      fileHs = mkModuleHs name
      templateHs = modulesDir </> template <.> "hs"
      targetDir = takeDirectory fileHs
  CommandList -> readPackageYaml $ \y1 _ _ -> do
    liftIO $ putDoc' "Executable ( source-dirs ):"
    let y2 =
          (y1 ^.. key executables . members . withIndex)
            <&> over
              _2
              ( \y ->
                  T.intercalate
                    ", "
                    ( (y ^.. key sourceDirs . values . _String)
                        <> (y ^.. key sourceDirs . _String)
                    )
              )
    traverse_ (\(name, path) -> liftIO $ putDoc' $ text (T.unpack name) <+> parens ("" <+> text (T.unpack path) <+> "")) y2
  CommandRemove {name} -> do
    throwIfBadName name
    ex <- liftIO $ doesFileExist fileHs
    liftIO $ unless ex $ throwIO $ Ex $ bb fileHs <+> "doesn't exist"
    liftIO $ putDoc' $ "Removing" <+> bb fileHs
    tRemoveFile fileHs (mapThrow ERemove FileHs fileHs)
    tRemoveDirWithEmptyParents targetDir (mapThrow ERemove Directory targetDir) (mapThrow EWrite Directory targetDir)
    readPackageYaml $ \y1 _ _ -> do
      let withoutExe x =
            Object $
              KM.fromHashMapText $
                HM.fromList (x ^.. members . withIndex . filtered (\(y, _) -> y /= exe))
          y2 = y1 & over (key executables) withoutExe
      writePackageYaml y2
    liftIO updateHsProjectFiles
    where
      fileHs = mkModuleHs name
      targetDir = takeDirectory fileHs
      exe = mkExe name
  CommandSet {name} -> do
    throwIfBadName name
    tWriteFile ghci (encodeUtf8 txtGhci) (mapThrow EWrite Ghci ghci)
    tWriteFile ghcid (encodeUtf8 txtGhcid) (mapThrow EWrite Ghci ghci)
    where
      txtGhci = T.pack $ ":set -isrc\n:load" <-> mkModuleHs name
      txtGhcid = T.pack "-W\n-r=:main"
  where
    main' = "main"
    sourceDirs = "source-dirs"
    executables = "executables"
    mkModuleHs x = modulesDir </> x </> mainHs
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
