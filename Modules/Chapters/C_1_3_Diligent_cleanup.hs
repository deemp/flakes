module C_1_3_Diligent_cleanup where

import C_0_Setup (getDataDir)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (allocate, runResourceT)
import GHC.IO.IOMode (IOMode (..))
import System.FilePath ((</>))
import System.IO qualified as IO
import C_1_2_Writing_to_a_file (greetingTxt)

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <-
        allocate
            (IO.openFile (dir </> greetingTxt) WriteMode)
            IO.hClose
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")