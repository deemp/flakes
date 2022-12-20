module C_10_Change () where

import C_8_Responding (sendResponse)
import C_9_Content_types (countHelloHtml, htmlOk, textOk)
import Control.Concurrent.Async as Async (replicateConcurrently_)
import Control.Concurrent.STM (
  TVar,
  atomically,
  modifyTVar',
  newTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Concurrent.STM.TVar (newTVarIO, readTVar)
import Control.Monad (replicateM)
import Control.Monad.STM (STM)
import Data.Text.Lazy qualified as LT
import Data.Time as Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.Natural (Natural)
import Network.Simple.TCP (HostPreference (..), serve)
import Text.Blaze.Html5 as Html ()

-- 10.1 STM

-- 10.2 Increment

increment :: TVar Natural -> STM Natural
increment tvar = modifyTVar' tvar (+ 1) >> readTVar tvar

-- 10.4 The counting server

countingServer :: IO ()
countingServer = do
  hitCounter <- newTVarIO (0 :: Natural)
  serve @IO HostAny "8000" \(s, _) -> do
    count <- atomically (increment hitCounter)
    sendResponse s (htmlOk (countHelloHtml count))

trySTM :: IO ()
trySTM = do
  x <- newTVarIO "Constantinopole"
  readTVarIO x >>= putStrLn
  atomically (writeTVar x "Istanbul")
  readTVarIO x >>= putStrLn

{-
>>>trySTM
-}

-- 10.6 Exercises

--- Ex 26

incrementNotAtomic :: TVar Natural -> IO Natural
incrementNotAtomic t = do
  count <- readTVarIO t
  atomically $ writeTVar t (count + 1)
  readTVarIO t

testIncrement :: (TVar Natural -> IO a) -> IO Natural
testIncrement inc = do
  x <- atomically (newTVar @Natural 0)
  Async.replicateConcurrently_ 10 (replicateM 1000 (inc x))
  readTVarIO x

--- Ex 27

timingServer :: IO ()
timingServer = do
  lastTime <- newTVarIO Nothing
  serve @IO HostAny "8000" \(s, _) -> do
    prevTime <- readTVarIO lastTime
    curTime <- Time.getCurrentTime
    atomically $ writeTVar lastTime (Just curTime)
    sendResponse s $
      textOk $
        LT.pack $
          show (Time.diffUTCTime <$> Just curTime <*> prevTime)