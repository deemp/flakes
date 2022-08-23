module MessageFormat where

import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Encode.Generic (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic (Encoding)

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, parseJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Prelude (($), class Show)
import Data.Maybe (Maybe (..))
import Data.Either (Either(..))
import Data.Show (show)

-- Client -> Server

newtype MessageFromClient = MessageFromClient
  { recipient :: Recipient,
    contents :: String
  }

newtype BroadcastMessageFromClient = BroadcastMessageFromClient
  { recipients :: Array Recipient,
    contents :: String
  }

-- When a user sends a message server knows this user's name
-- So this user only specifies the target recipient
data Recipient
  = Public {channel :: Channel}
  | Private {user :: User}

newtype LoginData = LoginData
  { userName :: String,
    password :: String
  }

data CommandToServer
  = Login {loginData :: LoginData}
  | SendMessage {messageFromClient :: MessageFromClient}
  | BroadcastMessage {broadcastMessageFromClient :: BroadcastMessageFromClient}
  | CreateChannel {channel :: Channel}
  | RemoveChannel {channel :: Channel}
  | JoinChannel {channel :: Channel}
  | InviteToChannel {broadcastMessageFromClient :: BroadcastMessageFromClient}
  | RemoveFromChannel {user :: User}

newtype User = User {userName :: String}

newtype Channel = Channel {channelName :: String} 

-- When a servers sends a message to its client
-- this client should know who has sent this message
-- even if this message is sent to a public channel
data Sender
  = PublicSender {channel :: Channel, user :: User}
  | PrivateSender {user :: User}

newtype MessageToClient = MessageToClient
  { sender :: Sender,
    contents :: String
  }

newtype ServerNotification = ServerNotification
  { contents :: String
  }

data MessageFromServer
  = ServerSent {serverSent :: ServerNotification}
  | ClientSent {clientSent :: MessageToClient}


-- Instances

-- easier inner type manipulation
-- see https://pursuit.purescript.org/packages/purescript-newtype/5.0.0/docs/Data.Newtype#t:Newtype
derive instance Newtype BroadcastMessageFromClient _
derive instance Newtype Channel _
derive instance Newtype LoginData _
derive instance Newtype MessageFromClient _
derive instance Newtype MessageToClient _
derive instance Newtype ServerNotification _
derive instance Newtype User _

-- generic show
-- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md#deriving-from-generic
derive instance Generic BroadcastMessageFromClient _
derive instance Generic Channel _
derive instance Generic CommandToServer _
derive instance Generic LoginData _
derive instance Generic MessageFromClient _
derive instance Generic MessageFromServer _
derive instance Generic MessageToClient _
derive instance Generic Recipient _
derive instance Generic Sender _
derive instance Generic ServerNotification _
derive instance Generic User _

tag::String
tag = "(tag)"

contents::String
contents = "(contents)"

enc :: Encoding
enc = {tagKey: tag, unwrapSingleArguments: true, valuesKey: Nothing}

instance EncodeJson BroadcastMessageFromClient where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson Channel where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson CommandToServer where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson LoginData where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson MessageFromClient where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson MessageFromServer where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson MessageToClient where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson Recipient where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson Sender where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson ServerNotification where encodeJson a = genericEncodeJsonWith enc a
instance EncodeJson User where encodeJson a = genericEncodeJsonWith enc a

instance DecodeJson BroadcastMessageFromClient where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson Channel where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson CommandToServer where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson LoginData where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson MessageFromClient where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson MessageFromServer where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson MessageToClient where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson Recipient where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson Sender where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson ServerNotification where decodeJson a = genericDecodeJsonWith enc a
instance DecodeJson User where decodeJson a = genericDecodeJsonWith enc a

bm :: BroadcastMessageFromClient
bm = BroadcastMessageFromClient {contents: "str", recipients: [recipient, recipient]}

recipient :: Recipient
recipient = Private {user : usr}

usr :: User
usr = User {userName : "ehy"}

show' ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → String
show' a = stringify $ encodeJson a

instance Show BroadcastMessageFromClient where show = show'
instance Show Channel where show = show'
instance Show CommandToServer where show = show'
instance Show LoginData where show = show'
instance Show MessageFromClient where show = show'
instance Show MessageFromServer where show = show'
instance Show MessageToClient where show = show'
instance Show Recipient where show = show'
instance Show Sender where show = show'
instance Show ServerNotification where show = show'
instance Show User where show = show'

inve :: String
inve = "{\"(tag)\":\"InviteToChannel\",\"broadcastMessageFromClient\":{\"(tag)\":\"BroadcastMessageFromClient\",\"recipients\":[{\"(tag)\":\"Private\",\"user\":{\"(tag)\":\"User\",\"userName\":\"ehy\"}}],\"contents\":\"str\"}}"

invd :: String
invd = 
  case parseJson inve of
    Right e -> stringify e
    Left e -> show e

-- Generics tutorial
-- https://harry.garrood.me/blog/write-your-own-generics/
-- we encode from generic repr into target
-- we decode our initial type into generic

-- Workflow with generics
-- https://jordanmartinez.github.io/purescript-jordans-reference-site/content/31-Design-Patterns/22-Generics.html


-- TODO
-- Can we read JSON and convert it into Generic representation?

-- When converting from Generic into JSON, we need to insert some specific expressions like `(tag)`
-- When converting from JSON into Generic, we need to eliminate them

-- we need then our own `to'` and `from'` functions to operate on 