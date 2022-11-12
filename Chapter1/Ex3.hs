module Ex3 where

import Prelude ()
import Relude

import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

import Control.Monad.Trans.Resource
    (ReleaseKey, ResourceT, allocate, runResourceT)

import qualified Control.Exception.Safe as Ex

getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

howManyHandles = runResourceT @IO do
    hs <- openManyHandles
    putStrLn ("Opened " <> show (length hs) <> " handles")


openManyHandles = go []
    where
        go hs = do
            r <- fileResourceMaybe
            case r of
                Nothing -> return hs
                Just h -> go (h : hs)

fileResourceMaybe = do
    dir <- liftIO getDataDir
    result <- Ex.tryIO do
        (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
        return (Just h)
    case result of
        Right x -> return x
        Left e -> do
            print (displayException e)
            return Nothing    