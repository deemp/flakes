{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Hashable
import Data.Text (Text)
import GHC.Generics (Generic)

data Message = Message
  { sender :: User,
    recipient :: Channel,
    contents :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Message

instance ToJSON Message

newtype User = User {userName :: Text} deriving (Show, Eq, Ord, Generic)

-- >>>encode $ User "name"
-- "{\"userName\":\"name\"}"

instance FromJSON User

instance ToJSON User

instance Hashable User

data Channel
  = Public Text
  | Private User
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Channel

instance ToJSON Channel

instance Hashable Channel

defaultChannel :: Channel
defaultChannel = Public "main"

data ClientCommand
  = Login Text
  | SendMessage Message
  | CreatePublicChannel Text
  deriving (Show, Eq, Generic)

instance FromJSON ClientCommand

instance ToJSON ClientCommand

data ServerCommand
  = NewMessage Message
  | ChannelList [Channel]
  | NewChannel Channel
  | RemoveChannel Channel
  deriving (Show, Eq, Generic)

instance FromJSON ServerCommand

instance ToJSON ServerCommand

-- TODO use constructor as a tag in messages
