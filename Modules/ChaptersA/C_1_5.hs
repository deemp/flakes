module C_1_5 where

import C_0 (getDataDir)
import Control.Exception (Exception (..))
import Control.Exception.Safe (catchIO)
import Control.Exception.Safe qualified as Ex
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import GHC.IO.Handle (Handle)
import GHC.IO.IOMode (IOMode (..))
import System.FilePath ((</>))
import System.IO qualified as IO
import C_1_2 (greetingTxt)

-- Ex 1

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

-- Ex 2

handlePrintTest :: IO ()
handlePrintTest = runResourceT do
  (_, p1) <- fileResource "hey" WriteMode
  liftIO $ print p1
  liftIO $ IO.hShow p1 >>= print

-- Ex 3

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