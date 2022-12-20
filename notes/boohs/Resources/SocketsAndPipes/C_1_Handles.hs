module C_1_Handles (fileResource, getDataDir, greetingTxt) where

import Control.Exception (Exception (..))
import Control.Exception.Safe qualified as Ex
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Data.Functor ((<&>))
import GHC.IO.Handle (Handle)
import GHC.IO.IOMode (IOMode (..))
import Relude (print, putStrLn, show)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO qualified as IO
import Prelude hiding (print, putStrLn, show)

-- 0 Setup

getDataDir :: IO FilePath
getDataDir = do
  dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
  Dir.createDirectoryIfMissing True dir
  return dir

-- 1.2 Writing to a file

greetingTxt :: IO.FilePath
greetingTxt = "greeting.txt"

writeGreetingFile :: IO ()
writeGreetingFile = do
  dir <- getDataDir
  h <- IO.openFile (dir </> greetingTxt) WriteMode
  IO.putStrLn ("handle: " <> show h)
  IO.hPutStrLn h "hello"
  IO.hClose h
  IO.putStrLn dir

-- 1.4 MonadIO

helloWorld :: MonadIO m => m ()
helloWorld = liftIO (IO.putStrLn "hello, world")

-- 1.5 Exercises

---- Ex 1

writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
  dir <- liftIO getDataDir
  (_releaseKey, h) <- fileResource (dir </> greetingTxt) WriteMode
  liftIO (IO.hPutStrLn h "hello")
  liftIO (IO.hPutStrLn h "world")

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource p m =
  allocate
    (IO.openFile p m)
    IO.hClose

---- Ex 2

handlePrintTest :: IO ()
handlePrintTest = runResourceT do
  (_, p1) <- fileResource "hey" WriteMode
  liftIO $ print p1
  liftIO $ IO.hShow p1 >>= print

---- Ex 3

howManyHandles :: IO ()
howManyHandles = runResourceT @IO do
  hs <- openManyHandles
  liftIO $ putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
  let openManyHandles_ xs =
        do
          x <- fileResourceMaybe
          case x of
            Just x' -> openManyHandles_ (x' : xs)
            Nothing -> return xs
  openManyHandles_ []

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  dir <- liftIO getDataDir
  result <- Ex.tryIO (fileResource (dir </> "b") WriteMode <&> snd <&> Just)
  case result of
    Right x -> return x
    Left e -> do
      liftIO $ print (displayException e)
      return Nothing

main :: IO ()
main = howManyHandles