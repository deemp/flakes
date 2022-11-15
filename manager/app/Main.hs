{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Exception (Exception, catch)
import Control.Exception.Base (SomeException, throwIO)
import Control.Lens (At (at), Identity, filtered, non, over, withIndex, (.=), (^..), (^?), _2)
import Control.Lens.Getter ((^.))
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
import Options.Applicative.Help (Doc, Pretty (pretty), bold, dot, hardline, putDoc, softline, (<+>))
import Options.Applicative.Help.Pretty (parens, text)
import System.Directory (doesFileExist)
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

-- modulesDir :: FilePath
modulesDir :: FilePath
modulesDir = "./Modules"

-- templatesDir :: FilePath
templatesDir :: FilePath
templatesDir = "./Templates"

packageYaml :: FilePath
packageYaml = "./package.yaml"

stackYaml :: String
stackYaml = "./stack.yaml"

defaultTemplate :: String
defaultTemplate = "Contest"

simpleMain :: String
simpleMain = templatesDir </> "SimpleMain.hs"

ghci :: FilePath
ghci = "./.ghci"

ghcid :: FilePath
ghcid = "./.ghcid"

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
    <> (softline <> bb _TEMPLATE_NAME <+> "refers to a template at" <+> bb (templatesDir </> _TEMPLATE_NAME <.> "hs") <> dot)

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

data Target = TargetModule | TargetTemplate deriving (Show, Eq)

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
    ("Error: the" <+> text _NAME <+> "or" <+> text _TEMPLATE_NAME <+> bb name <+> "is bad.")
      <+> "It should be of form 'A(/A)*' like 'A' or 'A/A', where 'A' is an alphanumeric sequence starting with an uppercase letter."

instance Show ProcessError where
  show :: ProcessError -> String
  show = show

instance Exception ProcessError

-- Parsers

templatesSubCommand :: Parser Command
templatesSubCommand = makeSubCommand TargetTemplate

modulesSubCommand :: Parser Command
modulesSubCommand = makeSubCommand TargetModule

generalCommand :: Parser GeneralCommand
generalCommand =
  -- TODO some description for modules command
  GeneralCommand TargetModule <$> modulesSubCommand
    <|> GeneralCommand TargetTemplate <$> subparser (f "template" templatesSubCommand "Manipulate templates" <> commandGroup "Template commands:")
  where
    f name' command' desc =
      command
        name'
        ( info
            (helper <*> command')
            ( headerDoc (Just header') <> fullDesc <> progDesc desc
            )
        )

initCommand :: Parser Command
initCommand = pure CommandInit

updateCommand :: Parser Command
updateCommand = pure CommandUpdate

setCommand :: Parser Command
setCommand = CommandSet <$> parseFileName

listCommand :: Parser Command
listCommand = pure CommandList

addCommand :: Parser Command
addCommand = CommandAdd <$> parseFileName <*> parseTemplate

removeCommand :: Parser Command
removeCommand = CommandRemove <$> parseFileName

parseFileName :: Parser String
parseFileName = argument str (metavar _NAME)

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

makeSubCommand :: Target -> Parser Command
makeSubCommand target =
  subparser
    ( f
        "add"
        addCommand
        ( ("Add a" <+> name <+> "at" <+> bbFullPath <> dot <> softline <> "It will also appear in the" <> bb packageYaml <> dot <> softline)
            <> eitherTarget
              ("You may create other modules at" <+> bbDirPath <> softline <> "and import them into" <+> bbFullPath)
              ""
              target
            <> dot <> hardline
        )
        <> f
          "rm"
          removeCommand
          ( ("Remove the" <+> dirOrTemplate <+> bbFullPath <> softline <> "and its empty parent directories" <> dot)
              <> (softline <> "This" <+> name <+> "will also be removed from" <+> bb packageYaml <> dot) <> hardline
          )
        <> f
          "list"
          listCommand
          ( ("List " <> name <> "s'" <+> bb "executables" <+> "and their" <+> bb "source-dirs" <> dot)
              <> (softline <> "These are also available in" <+> bb packageYaml <> dot) <> hardline
          )
        <> f
          "set"
          setCommand
          ( ("Set the" <+> name <+> bbFullPath)
              <> (softline <> "so that it's loaded when a" <+> bb "ghci" <+> "session starts" <> dot)
              <> (softline <> "When" <+> bb "ghcid" <+> "starts")
              <> (softline <> "it will run this " <> name <> "'s" <+> bb "main" <+> "function" <> dot)
              <> hardline
          )
        <> f
          "init"
          initCommand
          ("Initialize a managed" <+> bb "stack" <+> "project" <> hardline)
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
    dirPath = dir </> _NAME
    bbFullPath = bb (dirPath <> file <> ".hs")
    bbDirPath = bb dirPath
    f name' command' desc = command name' (info (helper <*> command') (fullDesc <> progDescDoc (Just desc)))
    (dir, name, dirOrTemplate, file) =
      case target of
        TargetModule -> (modulesDir, "module", "directory of", "/Main")
        TargetTemplate -> (templatesDir, "template", "template at", "")

-- | Select an option depending on the Target constructor
eitherTarget :: p -> p -> Target -> p
eitherTarget f g x =
  case x of
    TargetModule -> f
    TargetTemplate -> g

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

putDoc' :: Doc -> IO ()
putDoc' x = putDoc (x <> hardline)

-- | safely handle command
-- collect into a monoid and rethrow the exceptions that occur when doing or undoing actions
handleCommand :: GeneralCommand -> IO ()
handleCommand (GeneralCommand {..}) = runManaged $ case command_ of
  CommandInit -> do
    liftIO $ putDoc' $ "Creating" <+> bb templatesDir
    tCreateDir templatesDir (mapThrow EWrite Directory templatesDir) (mapThrow ERemove Directory templatesDir)
    liftIO $ putDoc' $ "Creating" <+> bb modulesDir
    tCreateDir modulesDir (mapThrow EWrite Directory modulesDir) (mapThrow ERemove Directory modulesDir)
    liftIO $ putDoc' $ "Writing" <+> bb packageYaml
    tWriteFile packageYaml initPackageYaml (mapThrow EWrite FileHs packageYaml)
    liftIO $ putDoc' $ "Writing" <+> bb stackYaml
    tWriteFile stackYaml initStackYaml (mapThrow EWrite FileHs stackYaml)
    liftIO $ putDoc' $ "Writing" <+> bb simpleMain
    tWriteFile simpleMain initSimpleMain (mapThrow EWrite FileHs simpleMain)
    liftIO $ putDoc' $ "Writing" <+> bb gitignore
    tWriteFile gitignore initGitIgnore (mapThrow EWrite FileHs gitignore)
  CommandUpdate -> liftIO updateHsProjectFiles
  CommandAdd {name, template} -> do
    throwIfBadName name
    throwIfBadName template
    readPackageYaml $ \y1 atKey nempty -> do
      let templateExe = mkTemplateExe template
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
      fileHs = mkTargetHs name
      templateHs = templatesDir </> template <.> "hs"
      targetDir = takeDirectory fileHs
  CommandList -> readPackageYaml $ \y1 _ _ -> do
    liftIO $ putDoc' "Executable ( source-dirs ):"
    let check x =
          T.split (== '.') x
            & Prelude.head
            & ( \y ->
                  if target == TargetModule
                    then y /= "." <> T.pack templatesDir
                    else y == T.pack templatesDir
              )
        y2 =
          (y1 ^.. key executables . members . withIndex . filtered (\(k, _) -> check k))
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
    when (target == TargetModule) $ tRemoveDirWithEmptyParents targetDir (mapThrow ERemove Directory targetDir) (mapThrow EWrite Directory targetDir)
    readPackageYaml $ \y1 _ _ -> do
      let withoutExe x =
            Object $
              KM.fromHashMapText $
                HM.fromList (x ^.. members . withIndex . filtered (\(y, _) -> y /= exe))
          y2 = y1 & over (key executables) withoutExe
      writePackageYaml y2
    liftIO updateHsProjectFiles
    where
      fileHs = mkTargetHs name
      targetDir = takeDirectory fileHs
      exe = mkExe name
  CommandSet {name} -> do
    throwIfBadName name
    tWriteFile ghci (encodeUtf8 txtGhci) (mapThrow EWrite Ghci ghci)
    tWriteFile ghcid (encodeUtf8 txtGhcid) (mapThrow EWrite Ghci ghci)
    where
      txtGhci = T.pack $ ":set -isrc\n:load" <-> mkTargetHs name
      txtGhcid = T.pack "-W\n-r=:main"
  where
    main' = "main"
    sourceDirs = "source-dirs"
    executables = "executables"
    targetTopDir = eitherTarget modulesDir templatesDir target
    mkTargetHs x = targetTopDir </> eitherTarget (x </> mainHs) (x <.> "hs") target
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
    mkExe x = T.pack $ eitherTarget "" "Templates." target <> replace (== pathSeparator) '.' x
    mkTemplateExe x = T.pack $ "Templates." <> replace (== pathSeparator) '.' x
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
