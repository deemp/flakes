{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Proxy (Proxy(..))
import System.Environment

import Servant.API
import Servant.API.WebSocket

import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Servant.Server (Server, serve)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS


import qualified Network.WebSockets as WS

import WebChat.Common

type API = "endpoint" :> WebSocket :<|> Raw

data AppState = AppState
  { users :: HM.HashMap User (TQueue ServerCommand)
  , channels :: HS.HashSet Channel
  }

newAppState :: AppState
newAppState = AppState HM.empty (HS.singleton defaultChannel)

main = do
  (d : _) <- getArgs
  state <- newTMVarIO newAppState
  let app = serve (Proxy @API) $ server state d
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings app

server :: TMVar AppState -> FilePath -> Server API
server state static = (websocketServer state) :<|> serveDirectoryWebApp static

websocketServer :: MonadIO m => TMVar AppState -> WS.Connection -> m ()
websocketServer state conn =  do
    void . liftIO $ do
      WS.forkPingThread conn 10
      queue <- newTQueueIO
      forkIO $ sendProcess queue conn
      recvProcess state queue conn

recvProcess :: MonadIO m => TMVar AppState -> TQueue ServerCommand -> WS.Connection -> m ()
recvProcess state queue conn = forever $ do
  msgData <- liftIO $ WS.receiveData conn
  let msg = A.decode @ClientCommand msgData
  liftIO . print $ msg
  case msg of
    Just command ->
      case command of
        Login userName -> do
          let user = User userName
              channel = Private user
          liftIO . atomically $ do
            st <- readTMVar state
            let newUsers = HM.insert user queue (users st)
                newChannels = HS.insert channel (channels st)
            swapTMVar state (AppState newUsers newChannels)

          queues <- liftIO . atomically $ getAllChannels state
          let command = NewChannel channel
          liftIO . atomically $ forM_ queues $ \q -> writeTQueue q command

        SendMessage msg -> sendMsg state msg
        GetChannelList -> sendChannelList state queue
        CreatePublicChannel name -> createChannel state name
    Nothing -> return ()

sendProcess :: MonadIO m => TQueue ServerCommand -> WS.Connection -> m ()
sendProcess queue conn = liftIO $ forever $ do
  msg <- atomically $ readTQueue queue
  liftIO . print $ A.encode msg
  WS.sendTextData conn $ A.encode msg

sendMsg :: MonadIO m => TMVar AppState -> Message -> m ()
sendMsg state msg@Message{..} = do
  queues <- liftIO . atomically $ getChannel state recepient
  let command = NewMessage msg
  liftIO . atomically $ forM_ queues $ \q -> writeTQueue q command

getChannel :: TMVar AppState -> Channel -> STM [TQueue ServerCommand]
getChannel state (Public _) = getAllChannels state
getChannel state (Private user) = maybeToList . HM.lookup user . users <$> readTMVar state

getAllChannels :: TMVar AppState -> STM [TQueue ServerCommand]
getAllChannels state = HM.elems . users <$> readTMVar state

sendChannelList :: MonadIO m => TMVar AppState -> TQueue ServerCommand -> m ()
sendChannelList state queue = liftIO . atomically $ do
  st <- readTMVar state
  let command = ChannelList $ HS.toList . channels $ st
  writeTQueue queue command

createChannel :: MonadIO m => TMVar AppState -> Text -> m ()
createChannel state name = do
  let channel = Public name
  queues <- liftIO . atomically $ do
    st <- readTMVar state
    let newSt = AppState (users st) (HS.insert channel $ channels st)
    void $ swapTMVar state newSt
    getAllChannels state

  let command = NewChannel channel
  liftIO . atomically $ forM_ queues $ \q -> writeTQueue q command
