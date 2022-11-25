module C_1_3 where

import C_0 (getDataDir)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (allocate, runResourceT)
import GHC.IO.IOMode (IOMode (..))
import System.FilePath ((</>))
import System.IO qualified as IO
import C_1_2 (greetingTxt)

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <-
        allocate
            (IO.openFile (dir </> greetingTxt) WriteMode)
            IO.hClose
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")