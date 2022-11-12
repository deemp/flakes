module Ex1 where

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

writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")
