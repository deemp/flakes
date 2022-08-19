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

module State
( AppState
, newAppState
, addUser
, removeUser
, addChannel
, removeChannel
, getChannelList
, getQueuesForChannel
, getAllQueues
) where

import Data.Maybe (maybeToList)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Control.Concurrent.STM (TQueue)

import Common

data AppState = AppState
  { users :: HM.HashMap User (TQueue ServerCommand)
  , channels :: HS.HashSet Channel
  }

newAppState :: AppState
newAppState = AppState HM.empty (HS.singleton defaultChannel)

addUser :: User -> TQueue ServerCommand -> AppState -> AppState
addUser user queue AppState{..} = AppState newUsers newChannels
  where
    channel = Private user
    newUsers = HM.insert user queue users
    newChannels = HS.insert channel channels

removeUser :: User -> AppState -> AppState
removeUser user AppState{..} = AppState newUsers newChannels
  where
    channel = Private user
    newUsers = HM.delete user users
    newChannels = HS.delete channel channels

addChannel :: Channel -> AppState -> AppState
addChannel channel AppState{..} = AppState users (HS.insert channel channels)

removeChannel :: Channel -> AppState -> AppState
removeChannel channel AppState{..} = AppState users (HS.delete channel channels)

getChannelList :: AppState -> [Channel]
getChannelList = HS.toList . channels

getQueuesForChannel :: Channel -> AppState -> [TQueue ServerCommand]
getQueuesForChannel (Public _) state = getAllQueues state
getQueuesForChannel (Private user) state = maybeToList . HM.lookup user . users $ state

getAllQueues :: AppState -> [TQueue ServerCommand]
getAllQueues state = HM.elems . users $ state
