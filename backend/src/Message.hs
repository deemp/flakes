{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Message () where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.TH
import Data.Hashable
import Data.Text (Text)
import GHC.Generics (Generic)

-- Client -> Server

data MessageFromClient = MessageFromClient
  { recipient :: Recipient,
    contents :: Text
  }
  deriving (Show, Eq, Generic)

data BroadcastMessageFromClient = BroadcastMessageFromClient
  { recipients :: [Recipient],
    contents :: Text
  }
  deriving (Show, Eq, Generic)

-- When a user sends a message server knows this user's name
-- So this user only specifies the target recipient
data Recipient
  = Public Channel
  | Private User
  deriving (Show, Eq, Ord, Generic)

data LoginData = LoginData
  { userName :: Text,
    password :: Text
  }
  deriving (Show, Eq, Ord, Generic)

data CommandToServer
  = Login LoginData
  | SendMessage MessageFromClient
  | BroadcastMessage BroadcastMessageFromClient
  | CreateChannel Channel
  | RemoveChannel Channel
  | JoinChannel Channel
  | InviteToChannel BroadcastMessageFromClient
  | RemoveFromChannel User
  deriving (Show, Eq, Generic)

newtype User = User {userName :: Text} deriving (Show, Eq, Ord, Generic)

newtype Channel = Channel {channelName :: Text} deriving (Show, Eq, Ord, Generic)

-- Server -> Client

-- When a servers sends a message to its client
-- this client should know who has sent this message
-- even if this message is sent to a public channel
data Sender
  = PublicSender {channel :: Channel, user :: User}
  | PrivateSender {user :: User}
  deriving (Show, Eq, Ord, Generic)

data MessageToClient = MessageToClient
  { sender :: Sender,
    contents :: Text
  }
  deriving (Show, Eq, Generic)

newtype ServerNotification = ServerNotification
  { contents :: Text
  }
  deriving (Show, Eq, Generic)

data MessageFromServer
  = ServerSent ServerNotification
  | ClientSent MessageToClient
  deriving (Show, Eq, Generic)

-- Instances

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''Channel)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''User)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''LoginData)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''Recipient)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''Sender)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''BroadcastMessageFromClient)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''ServerNotification)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''MessageToClient)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''MessageFromClient)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''MessageFromServer)

$(deriveJSON defaultOptions {tagSingleConstructors = True} ''CommandToServer)

-- Internal

-- We'll need to store users
instance Hashable User


defaultChannel :: Recipient
defaultChannel = Public $ Channel "main"

pc = encode $ Public $ Channel "us"

pu = encode $ Private $ User "us"

{-
>>>pc
"{\"tag\":\"Public\",\"contents\":{\"tag\":\"Channel\",\"channelName\":\"us\"}}"
>>>pu
"{\"tag\":\"Private\",\"contents\":{\"tag\":\"User\",\"userName\":\"us\"}}"
-}

us = encode $ User "hey"

un = decode "{\"tag\":\"User\",\"userName\":\"hey\"}" :: Maybe User

{-
>>>us
"{\"tag\":\"User\",\"userName\":\"hey\"}"
>>>un
Just (User {userName = "hey"})
-}
