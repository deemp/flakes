{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module State
  ( AppState,
    -- newAppState,
    addUser,
    banUser,
    -- addChannel,
    -- removeChannel,
    -- getChannelList,
    -- getQueuesForChannel,
    getAllQueues,
  )
where

import Control.Concurrent.STM (TQueue)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (maybeToList)
import Data.Text
import TransportTypes


-- TODO smart constructor for user
-- - check user name valid
-- - check user name available
-- data PosInt where
--   Pos :: 

data ChannelData = ChannelData
  { id :: Int,
    channel :: Channel,
    admins :: HS.HashSet User
  }

data MemberMedium = MemberMedium {
  medium :: Medium,
  permissions :: Permissions
}

data AppState = AppState
  { -- a queue of commands for the server to execute per each user
    -- commands are kept for independent sessions
    userCommands :: HM.HashMap User (TQueue ClientRequest),
    userPasswords :: HM.HashMap User Text,
    channels :: HS.HashSet ChannelData,
    usersOnline :: HS.HashSet User,
    -- A user can create channels and invite people there
    -- If a creator leaves a channel the channel will be GC-d
    userChannels :: HM.HashMap User (HS.HashSet Channel)
  }

-- Like Telegram channel
-- defaultChannel :: ChannelData
-- defaultChannel = ChannelData {channel = Channel "main", admins = HS.empty}

-- newAppState :: AppState
-- newAppState =
--   AppState
--     { userCommands = HM.empty,
--       channels = HS.singleton defaultChannel,
--       userChannels = HM.empty,
--       usersOnline = HS.empty,
--       userPasswords = HM.empty
--     }
 
addUser :: User -> TQueue ClientRequest -> AppState -> AppState
addUser user queue t@AppState {..} =
  t
    { userCommands = HM.insertWith (\new old -> old) user queue userCommands,
      userChannels = HM.insertWith (\new old -> old) user HS.empty userChannels
    }

banUser :: User -> Channel -> AppState -> AppState
banUser user channel t@AppState {..} =
  t
    { userChannels = HM.update (Just . HS.delete channel) user userChannels
    }

-- TODO when a user creates a channel
-- it adds a channel
-- and joins the channel

-- addChannel :: Channel -> AppState -> AppState
-- addChannel channel t@AppState {..} = t {channels = HS.insert channel channels}

-- removeChannel :: Channel -> AppState -> AppState
-- removeChannel channel t@AppState {..} = t {channels = HS.delete channel channels}

-- getChannelList :: AppState -> [Channel]
-- getChannelList state = state & channels & HS.toList

-- getQueuesForChannel :: Channel -> AppState -> [TQueue ClientRequest]
-- getQueuesForChannel (Public _) state = getAllQueues state
-- getQueuesForChannel (Private user) state = state & userCommands & HM.lookup user & maybeToList

getAllQueues :: AppState -> [TQueue ClientRequest]
getAllQueues state = state & userCommands & HM.elems
