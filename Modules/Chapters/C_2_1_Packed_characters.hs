module C_2_1_Packed_characters where

import C_0_Setup (getDataDir)
import C_1_5_Exercises (fileResource)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle.FD (stdout)
import GHC.IO.IOMode (IOMode (WriteMode))
import System.FilePath ((</>))
import C_1_2_Writing_to_a_file (greetingTxt)

helloText :: IO ()
helloText = T.hPutStrLn stdout (T.pack "hello, world!")

helloTextFile :: IO ()
helloTextFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> greetingTxt) WriteMode
  liftIO do
    T.hPutStrLn h (T.pack "hello")
    T.hPutStrLn h (T.pack "world")

-- Text is strict

-- this crashes
p :: T.Text
p = T.take 10 (T.pack (cycle "abc"))