module C_2_2_Reading_from_a_file(repeatUntilIO) where

import C_0_Setup (getDataDir)
import C_1_5_Exercises (fileResource)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Relude (IOMode (ReadMode))
import System.FilePath ((</>))
import System.IO qualified as IO
import C_1_2_Writing_to_a_file (greetingTxt)

-- We read by chunks
printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> greetingTxt) ReadMode
  liftIO $
    repeatUntilIO (T.hGetChunk h) T.null $
      \chunk -> T.putStr (T.toUpper chunk)

printCapitalizedText :: IO.Handle -> IO ()
printCapitalizedText h = continue
 where
  continue = do
    chunk <- T.hGetChunk h
    unless
      (T.null chunk)
      ( do
          T.putStr (T.toUpper chunk)
          continue
      )

repeatUntilIO ::
  IO chunk ->
  (chunk -> Bool) ->
  (chunk -> IO x) ->
  IO ()
repeatUntilIO getChunk isEnd f = continue
 where
  continue = do
    chunk <- getChunk
    unless
      (isEnd chunk)
      ( do
          _ <- f chunk
          continue
      )