{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Message (inve) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Data (Proxy (Proxy))
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import MessageTH (options)

-- Client -> Server

data MessageFromClient = MessageFromClient
  { recipient :: Recipient,
    contents :: Text
  }
  deriving (Show, Eq, Ord, Generic)

data BroadcastMessageFromClient = BroadcastMessageFromClient
  { recipients :: [Recipient],
    contents :: Text
  }
  deriving (Show, Eq, Ord, Generic)

-- When a user sends a message server knows this user's name
-- So this user only specifies the target recipient
data Recipient
  = Public {channel :: Channel}
  | Private {user :: User}
  deriving (Show, Eq, Ord, Generic)

data LoginData = LoginData
  { userName :: Text,
    password :: Text
  }
  deriving (Show, Eq, Ord, Generic)

data CommandToServer
  = Login {loginData :: LoginData}
  | SendMessage {messageFromClient :: MessageFromClient}
  | BroadcastMessage {broadcastMessageFromClient :: BroadcastMessageFromClient}
  | CreateChannel {channel :: Channel}
  | RemoveChannel {channel :: Channel}
  | JoinChannel {channel :: Channel}
  | InviteToChannel {broadcastMessageFromClient :: BroadcastMessageFromClient}
  | RemoveFromChannel {user :: User}
  deriving (Show, Eq, Ord, Generic)

newtype User = User {userName :: Text}
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype Channel = Channel {channelName :: Text}
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

newtype ServerNotification = ServerNotification
  { contents :: Text
  }
  deriving (Show, Eq, Ord, Generic)

data MessageFromServer
  = ServerSent {serverSent :: ServerNotification}
  | ClientSent {clientSent :: MessageToClient}
  deriving (Show, Eq, Ord, Generic)

$(deriveJSON options ''Channel)

$(deriveJSON options ''User)

$(deriveJSON options ''LoginData)

$(deriveJSON options ''Recipient)

$(deriveJSON options ''Sender)

$(deriveJSON options ''BroadcastMessageFromClient)

$(deriveJSON options ''ServerNotification)

$(deriveJSON options ''MessageToClient)

$(deriveJSON options ''MessageFromClient)

$(deriveJSON options ''MessageFromServer)

$(deriveJSON options ''CommandToServer)

r :: Recipient
r = Private usr

usr :: User
usr = User {userName = "ehy"}

bm :: BroadcastMessageFromClient
bm = BroadcastMessageFromClient {contents = "str", recipients = [r]}

inv :: CommandToServer
inv = InviteToChannel bm

bme' = encode bm

encR = encode r

encUsr = encode usr

bmp = "{\"recipients\":[{\"user\":{\"userName\":\"ehy\",\"(tag)\":\"User\"},\"(tag)\":\"Private\"},{\"user\":{\"userName\":\"ehy\",\"(tag)\":\"User\"},\"(tag)\":\"Private\"}],\"contents\":\"str\",\"(tag)\":\"BroadcastMessageFromClient\"}"

inve = "{\"(tag)\":\"InviteToChannel\",\"broadcastMessageFromClient\":{\"(tag)\":\"BroadcastMessageFromClient\",\"recipients\":[{\"(tag)\":\"Private\",\"user\":{\"(tag)\":\"User\",\"userName\":\"ehy\"}}],\"contents\":\"str\"}}"

invd :: Maybe CommandToServer
invd = decode inve

bmpd :: Maybe BroadcastMessageFromClient
bmpd = decode bmp

-- inve = encode inv

{-
>>>bmpd
Just (BroadcastMessageFromClient {recipients = [Private {user = User {userName = "ehy"}},Private {user = User {userName = "ehy"}}], contents = "str"})
>>>inve
"{\"(tag)\":\"InviteToChannel\",\"broadcastMessageFromClient\":{\"(tag)\":\"BroadcastMessageFromClient\",\"recipients\":[{\"(tag)\":\"Private\",\"user\":{\"(tag)\":\"User\",\"userName\":\"ehy\"}}],\"contents\":\"str\"}}"
-}
