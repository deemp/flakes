{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module WebChat.Common where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Data.Hashable
import Data.Text (Text)


data Message = Message
  { sender :: User
  , recepient :: Channel
  , contents :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Message
instance ToJSON Message


newtype User = User { userName :: Text } deriving (Show, Eq, Ord, Generic)

instance FromJSON User
instance ToJSON User
instance Hashable User


data Channel
  = Public Text | Private User
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Channel
instance ToJSON Channel
instance Hashable Channel

defaultChannel :: Channel
defaultChannel = Public "main"


data ClientCommand
  = Login Text
  | SendMessage Message
  | GetChannelList
  | CreatePublicChannel Text
  deriving (Show, Eq, Generic)

instance FromJSON ClientCommand
instance ToJSON ClientCommand


data ServerCommand
  = NewMessage Message
  | ChannelList [Channel]
  | NewChannel Channel
--  | RemoveChannel Channel
  deriving (Show, Eq, Generic)

instance FromJSON ServerCommand
instance ToJSON ServerCommand
