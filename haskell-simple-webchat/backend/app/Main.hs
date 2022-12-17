{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

-- import Common
--   ( Channel (..),
--     ClientCommand (..),
--     Message (..),
--     ServerCommand (..),
--     User (User),
--   )

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    myThreadId,
  )
import Control.Concurrent.STM
  ( STM,
    TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVar,
    writeTQueue,
  )
import Control.Exception (finally)
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (MonadIO (..))
-- import qualified Data.Aeson as A
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.Wai.Application.Static (defaultWebAppSettings, ssIndices)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import qualified Network.WebSockets as WS
import Servant.API (Raw, type (:<|>) (..), type (:>))
import Servant.API.WebSocket (WebSocket)
import Servant.Server (Server, serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import qualified State
import System.Environment (getArgs)
import WaiAppStatic.Types (unsafeToPiece)

type API = "endpoint" :> WebSocket :<|> Raw

main :: IO ()
main = do
  print "help!"
  -- takes filepath and port as arguments
  -- filepath to an html
  -- (path : port' : _) <- getArgs
  -- state <- newTVarIO State.newAppState
  -- let app = serve (Proxy @API) $ server state path
  --     port = read @Int port'
  -- withStdoutLogger $ \logger -> do
  --   putStrLn $ "Server is listening at port " ++ show port
  --   let settings = setPort port $ setLogger logger defaultSettings
  --   runSettings settings app

-- server :: TVar State.AppState -> FilePath -> Server API
-- server state static = websocketServer state :<|> serveDirectoryWith staticSettings
--   where
--     staticSettings = (defaultWebAppSettings static) {ssIndices = [unsafeToPiece "index.html"]}

-- websocketServer :: MonadIO m => TVar State.AppState -> WS.Connection -> m ()
-- websocketServer state conn = do
--   void . liftIO $ do
--     -- WTF should be the IO a here?
--     WS.withPingThread conn 10 (return ()) $ do
--       queue <- newTQueueIO
--       recvProcess state queue conn

-- onJust :: Maybe a -> b -> (a -> b) -> b
-- onJust m d f = maybe d f m

-- recvProcess :: TVar State.AppState -> TQueue ServerCommand -> WS.Connection -> IO ()
-- recvProcess state queue conn = do
--   let loginLoop :: IO User
--       loginLoop = do
--         msg <- getMessage
--         onJust msg loginLoop $
--           \case
--             Login userName -> atomically $ do
--               let user = User userName
--                   channel = Private user
--               modifyTVar' state $ State.addUser user queue
--               sendChannelList state queue
--               queues <- State.getAllQueues <$> readTVar state
--               send queues (NewChannel channel)
--               return user
--             _ -> loginLoop

--   user <- loginLoop
--   recvThId <- myThreadId
--   sendThId <- forkIO $ sendProcess user state recvThId queue conn

--   flip finally (disconnect state user sendThId) $
--     forever $ do
--       message <- getMessage
--       atomically $
--         onJust message (return ()) $
--           \case
--             SendMessage msg -> sendMessage state msg
--             CreatePublicChannel name -> createChannel state name
--             _ -> return ()
--   where
--     -- try to read a message from WS else block
--     -- decode a message as a ClientCommand
--     -- TODO change message format
--     getMessage :: IO (Maybe ClientCommand)
--     getMessage = return Nothing
--     -- getMessage = A.decode @ClientCommand <$> WS.receiveData conn

-- sendProcess :: User -> TVar State.AppState -> ThreadId -> TQueue ServerCommand -> WS.Connection -> IO ()
-- sendProcess user state recvThId queue conn =
--   flip finally (disconnect state user recvThId) $
--     forever $ do
--       msg <- atomically $ readTQueue queue
--       -- WS.sendTextData conn $ A.encode msg
--       WS.sendTextData conn ("" :: Text)

-- send :: [TQueue ServerCommand] -> ServerCommand -> STM ()
-- send queues command = forM_ queues $ flip writeTQueue command

-- sendMessage :: TVar State.AppState -> Message -> STM ()
-- sendMessage state msg@Message {} = do
--   state' <- readTVar state
--   let queues = foldMap (`State.getQueuesForChannel` state') (getChannelsForMessage msg)
--   send queues $ NewMessage msg
--   where
--     getChannelsForMessage :: Message -> [Channel]
--     getChannelsForMessage (Message {recipient = ch@(Public {})}) = [ch]
--     getChannelsForMessage (Message {sender, recipient = ch@(Private {})}) = [ch, Private sender]

-- sendChannelList :: TVar State.AppState -> TQueue ServerCommand -> STM ()
-- sendChannelList state queue = do
--   command <- ChannelList . State.getChannelList <$> readTVar state
--   writeTQueue queue command

-- createChannel :: TVar State.AppState -> Text -> STM ()
-- createChannel state name = do
--   let channel = Public name
--   modifyTVar' state $ State.addChannel channel
--   queues <- State.getAllQueues <$> readTVar state
--   send queues $ NewChannel channel

-- disconnect :: TVar State.AppState -> User -> ThreadId -> IO ()
-- disconnect state user thId = do
--   atomically $ do
--     modifyTVar' state $ State.removeUser user
--     queues <- State.getAllQueues <$> readTVar state
--     send queues $ RemoveChannel $ Private user
--   -- TODO ensure client disconnected
--   -- maybe send it a message
--   -- and wait until it confirms disconnect
--   -- then kill its thread
--   killThread thId
