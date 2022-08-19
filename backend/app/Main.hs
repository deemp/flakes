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
import Control.Exception (finally)
import Data.Proxy (Proxy(..))
import System.Environment

import Servant.API
import Servant.API.WebSocket
import Servant.Server.StaticFiles (serveDirectoryWith)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import WaiAppStatic.Types (unsafeToPiece)
import Servant.Server (Server, serve)
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import qualified Network.WebSockets as WS

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Aeson as A
import Data.Text (Text)

import Common
import qualified State

type API = "endpoint" :> WebSocket :<|> Raw

main :: IO ()
main = do
  (d : p : _) <- getArgs
  state <- newTVarIO State.newAppState
  let app = serve (Proxy @API) $ server state d
      port = read @Int p
  withStdoutLogger $ \aplogger -> do
    putStrLn $ "Server is listening at port " ++ show port
    let settings = setPort port $ setLogger aplogger defaultSettings
    runSettings settings app

server :: TVar State.AppState -> FilePath -> Server API
server state static = (websocketServer state) :<|> serveDirectoryWith staticSettings
  where
    staticSettings = (defaultWebAppSettings static) {ssIndices = [unsafeToPiece "index.html"]}

websocketServer :: MonadIO m => TVar State.AppState -> WS.Connection -> m ()
websocketServer state conn = do
    void . liftIO $ do
      WS.forkPingThread conn 10
      queue <- newTQueueIO
      recvProcess state queue conn

recvProcess :: TVar State.AppState -> TQueue ServerCommand -> WS.Connection -> IO ()
recvProcess state queue conn = do
  let
    loginLoop :: IO User
    loginLoop = do
      msg <- getMessage
      case msg of
        Just command ->
          case command of
            Login userName -> atomically $ do
              let user = User userName
                  channel = Private user
              modifyTVar' state $ State.addUser user queue
              sendChannelList state queue
              queues <- State.getAllQueues <$> readTVar state
              send queues (NewChannel channel)
              return user
            _ -> loginLoop
        Nothing -> loginLoop

  user <- loginLoop
  recvThId <- myThreadId
  sendThId <- forkIO $ sendProcess user state recvThId queue conn

  flip finally (disconnect state user sendThId) $
    forever $ do
      message <- getMessage
      atomically $
        case message of
          Just command ->
            case command of
              SendMessage msg -> sendMessage state msg
              CreatePublicChannel name -> createChannel state name
              _ -> return ()
          Nothing -> return ()
  where
    getMessage :: IO (Maybe ClientCommand)
    getMessage = A.decode @ClientCommand <$> WS.receiveData conn

sendProcess :: User -> TVar State.AppState -> ThreadId -> TQueue ServerCommand -> WS.Connection -> IO ()
sendProcess user state recvThId queue conn =
  flip finally (disconnect state user recvThId) $
    forever $ do
      msg <- atomically $ readTQueue queue
      void $ WS.sendTextData conn $ A.encode msg

send :: [TQueue ServerCommand] -> ServerCommand -> STM ()
send queues command = forM_ queues $ flip writeTQueue command

sendMessage :: TVar State.AppState -> Message -> STM ()
sendMessage state msg@Message{..} = do
  state' <- readTVar state
  let queues = foldMap (flip State.getQueuesForChannel state') (getChannelsForMessage msg)
  send queues $ NewMessage msg
  where
    getChannelsForMessage :: Message -> [Channel]
    getChannelsForMessage (Message _ ch@(Public _) _) = [ch]
    getChannelsForMessage (Message sender' ch@(Private _) _) = [ch, Private sender']

sendChannelList :: TVar State.AppState -> TQueue ServerCommand -> STM ()
sendChannelList state queue = do
  command <- ChannelList . State.getChannelList <$> readTVar state
  writeTQueue queue command

createChannel :: TVar State.AppState -> Text -> STM ()
createChannel state name = do
  let channel = Public name
  modifyTVar' state $ State.addChannel channel
  queues <- State.getAllQueues <$> readTVar state
  send queues $ NewChannel channel

disconnect :: TVar State.AppState -> User -> ThreadId -> IO ()
disconnect state user thId = do
  atomically $ do
    modifyTVar' state $ State.removeUser user
    queues <- State.getAllQueues <$> readTVar state
    send queues $ RemoveChannel $ Private user
  killThread thId
