{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module State
  ( AppState,
    newAppState,
    addUser,
    removeUser,
    addChannel,
    removeChannel,
    getChannelList,
    getQueuesForChannel,
    getAllQueues,
  )
where

import Common
import Control.Concurrent.STM (TQueue)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (maybeToList)

data AppState = AppState
  { users :: HM.HashMap User (TQueue ServerCommand),
    channels :: HS.HashSet Channel
  }

newAppState :: AppState
newAppState = AppState HM.empty (HS.singleton defaultChannel)

addUser :: User -> TQueue ServerCommand -> AppState -> AppState
addUser user queue AppState {..} = AppState newUsers newChannels
  where
    channel = Private user
    newUsers = HM.insert user queue users
    newChannels = HS.insert channel channels

removeUser :: User -> AppState -> AppState
removeUser user AppState {..} = AppState newUsers newChannels
  where
    channel = Private user
    newUsers = HM.delete user users
    newChannels = HS.delete channel channels

addChannel :: Channel -> AppState -> AppState
addChannel channel AppState {..} = AppState users (HS.insert channel channels)

removeChannel :: Channel -> AppState -> AppState
removeChannel channel AppState {..} = AppState users (HS.delete channel channels)

getChannelList :: AppState -> [Channel]
getChannelList state = state & channels & HS.toList

getQueuesForChannel :: Channel -> AppState -> [TQueue ServerCommand]
getQueuesForChannel (Public _) state = getAllQueues state
getQueuesForChannel (Private user) state = state & users & HM.lookup user & maybeToList

getAllQueues :: AppState -> [TQueue ServerCommand]
getAllQueues state = state & users & HM.elems
