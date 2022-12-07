module ExMonoid where

import Control.Applicative ((<|>))
import Control.Exception (Exception, catch, handle)
import Control.Exception.Base
  ( Exception (fromException),
    SomeException,
    mask,
    throwIO,
  )
import Control.Monad.Except
  ( MonadIO (liftIO),
    unless,
    when,
  )
import Control.Monad.Managed (MonadManaged, managed, runManaged)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Options.Applicative.Help (Doc, Pretty (..), vsep)
import Options.Applicative.Help.Pretty (string)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectory,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
    renameFile,
  )
import System.FilePath (splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO.Temp (createTempDirectory)

newtype ExMonoid = ExMonoid [SomeException]

instance Show ExMonoid where
  show :: ExMonoid -> String
  show (ExMonoid s) = show s

instance Pretty ExMonoid where
  pretty :: ExMonoid -> Doc
  pretty (ExMonoid s) = vsep $ pretty . PrettyException <$> s

newtype PrettyException = PrettyException SomeException deriving (Show)

instance Pretty PrettyException where
  pretty :: PrettyException -> Doc
  pretty (PrettyException ex) =
    fromMaybe
      (string $ show ex)
      ( (pretty <$> fromException @Ex ex)
          <|> (pretty <$> fromException @ExMonoid ex)
      )

instance Exception ExMonoid

fmapE :: forall e a. Exception e => (e -> a) -> ExMonoid -> [Maybe a]
fmapE f (ExMonoid s) = (f <$>) . fromException <$> s

filterMapE :: forall e a. Exception e => (e -> a) -> ExMonoid -> [a]
filterMapE f (ExMonoid s) = Prelude.foldr (\x m -> maybe m ((: m) . f) (fromException @e x)) [] s

bracket' :: IO t -> (t -> IO a) -> (t -> IO b) -> IO b
bracket' before after thing =
  mask $ \restore ->
    catchThrow
      ( do
          a <- before
          r <- catchThrow (restore (thing a)) `onException'` after a
          _ <- after a
          return r
      )

mBracket :: MonadManaged m => p -> (p -> IO a1) -> (a1 -> b) -> (b -> IO a2) -> m a1
mBracket x f y g = managed (bracket' (f x) (g . y))

mAfter :: MonadManaged m => b -> (b -> IO a2) -> m ()
mAfter x = mBracket (return ()) id (const x)

-- | Like the original bracketOnError, but assumes that any action can cause an exception
-- collects all exceptions into a monoid
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

catchThrow :: IO a -> IO a
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

-- | managed bracketOnError'
mBracketOnError :: MonadManaged m => p -> (p -> IO a) -> (a -> b1) -> (b1 -> IO b2) -> m a
mBracketOnError x f y g = managed (bracketOnError' (f x) (g . y))

-- | managed bracketOnError' that doesn't perform anything after using a resource
mBefore :: MonadManaged m => p -> (p -> IO a) -> m a
mBefore x f = mBracketOnError x f (const (return ())) id

-- | managed bracketOnError' that performs something after using a resource on error
mAfterOnError :: (MonadManaged m1, Monad m2) => IO b -> (m2 () -> IO a) -> m1 a
mAfterOnError x f = mBracketOnError (return ()) f (const x) id

tReadFile :: MonadManaged m => FilePath -> (IO BS.ByteString -> IO a) -> m a
tReadFile x = mBefore (BS.readFile x)

-- | safely write a file
--
-- only map the write exception
--
-- TODO allow to map other exceptions
tWriteFile :: MonadManaged m => FilePath -> BS.ByteString -> (IO () -> IO b) -> m b
tWriteFile path new write_ = do
  -- we check if such file exists
  ex <- liftIO $ doesFileExist path
  _ <-
    if ex
      then do
        -- if it exists, we make a backup
        let bk = path <> ".bk"
        -- we remove the backup file if it wasn't removed due to renaming
        mAfter
          ( do
              ef <- doesFileExist bk
              when ef (removeFile bk)
          )
          id
        -- in case of an error, we write that file back
        mBracketOnError (copyFile path bk) id (const $ renameFile bk path) id
      else do
        -- if there was no file, in case of errors we should remove the new file
        mAfterOnError (removeFile path) id
  -- finally, we write our new contents
  mBefore (BS.writeFile path new) write_

-- | safely remove a file
--
-- only map the remove exception
--
-- TODO allow to map other exceptions
tRemoveFile :: MonadManaged m => FilePath -> (IO () -> IO ()) -> m ()
tRemoveFile path remove_ = do
  -- we check if such file exists
  ex <- liftIO $ doesFileExist path
  when ex $ do
    -- if it exists, we make a backup
    let bk = "bk"
        fname = takeFileName path
    tmp <- mBefore (createTempDirectory "." bk) id
    -- we should anyway remove the backup file
    mAfter (removeDirectoryRecursive tmp) id
    let bkPath = tmp </> fname
    -- we create a backup file and restore the original file on exception
    mBracketOnError (copyFile path bkPath) id (const $ renameFile bkPath path) id
    -- we remove the file
    mBefore (removeFile path) remove_

tCreateDir :: MonadManaged m => FilePath -> (IO () -> IO b) -> (IO () -> IO b2) -> m b
tCreateDir path f g = do
  -- we remember if such directory exists
  ex <- liftIO $ doesDirectoryExist path
  mBracketOnError (createDirectoryIfMissing True path) f (\_ -> unless ex (removeDirectory path)) g

tRemoveDirWithEmptyParents :: MonadManaged m => FilePath -> (IO () -> IO ()) -> (IO () -> IO b2) -> m ()
tRemoveDirWithEmptyParents path f g = do
  -- we remember if such directory exists
  let removeDirectoryIfEmpty dir = do
        isEmptyDir <- Prelude.null <$> listDirectory dir
        when isEmptyDir (removePathForcibly dir)
      targetDirParents =
        let nParents = length (splitDirectories path)
         in -- take directories apart from . and target
            tail $ take (max 0 (nParents - 1)) (iterate takeDirectory path)
  ex <- liftIO $ doesDirectoryExist path
  when ex $
    mBracketOnError
      (removePathForcibly path >> traverse_ removeDirectoryIfEmpty targetDirParents)
      f
      -- restore the contents
      (\_ -> createDirectoryIfMissing True path)
      g

do' :: String -> IO ()
do' x = putStrLn ("do " <> x)

undo' :: String -> IO ()
undo' x = putStrLn ("undo " <> x)

newtype Ex = Ex Doc deriving (Show)

instance Exception Ex

instance Pretty Ex where
  pretty :: Ex -> Doc
  pretty (Ex x) = x

-- | demo collect exceptions into a monoid
tryManaged' :: IO ()
tryManaged' = handle (\(x :: ExMonoid) -> print x) $
  runManaged $ do
    _ <- mBracketOnError (do' "A" >> throwIO (Ex "Arelease")) (\_ -> undo' "A" >> throwIO (Ex "Arelease")) id id
    _ <- mBracketOnError (do' "B") (\_ -> undo' "B" >> throwIO (Ex "B")) id id
    _ <- mBracketOnError (do' "C") (\_ -> undo' "C" >> throwIO (Ex "C")) id id
    _ <- mBracketOnError (do' "D" >> throwIO (Ex "Da")) (\_ -> undo' "D" >> throwIO (Ex "Dr")) id id
    liftIO $ throwIO $ Ex "final1"
