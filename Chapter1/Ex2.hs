module Ex2 where

import Prelude ()
import Relude

import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

import Control.Monad.Trans.Resource
    (ReleaseKey, ResourceT, allocate, runResourceT)

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

handlePrintTest = runResourceT @IO do
    (_, fh1) <- fileResource "/tmp/show-test-1" WriteMode
    (_, fh2) <- fileResource "/tmp/show-test-2" ReadWriteMode
    liftIO $ for_ [stdin, stdout, stderr, fh1, fh2] \h -> do
        IO.putStrLn (show h)
        info <- IO.hShow h
        IO.putStrLn info 