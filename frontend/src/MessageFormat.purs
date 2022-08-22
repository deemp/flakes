module MessageFormat where

import Data.Argonaut.Decode.Generic
import Data.Argonaut.Encode.Generic
import Data.Argonaut.Types.Generic

import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic, Constructor(..), Product(..), Sum(..), from)
import Data.Lens.Lens.Product (_1)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Prelude (($), class Show)
import Type.Proxy (Proxy(..))


class ConstrName rep where
  constrName' :: rep -> String

instance IsSymbol name => ConstrName (Constructor name a) where
  constrName' (Constructor _) = reflectSymbol (Proxy :: Proxy name)

instance (ConstrName a, ConstrName b) => ConstrName (Sum a b) where
  constrName' (Inl a) = constrName' a
  constrName' (Inr b) = constrName' b

instance (ConstrName a, ConstrName b) => ConstrName (Product a b) where
  constrName' (Product a b) = constrName' b

constrName :: forall a rep. Generic a rep => ConstrName rep => a -> String
constrName a = constrName' $ from a

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
  = Public Channel
  | Private User

newtype LoginData = LoginData
  { userName :: String,
    password :: String
  }

data CommandToServer
  = Login LoginData
  | SendMessage MessageFromClient
  | BroadcastMessage BroadcastMessageFromClient
  | CreateChannel Channel
  | RemoveChannel Channel
  | JoinChannel Channel
  | InviteToChannel BroadcastMessageFromClient
  | RemoveFromChannel User

newtype User = User {userName :: String}

newtype Channel = Channel {channelName :: String} 



-- r = constrName A
-- Server -> Client

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
  = ServerSent ServerNotification
  | ClientSent MessageToClient


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

addTag :: forall a rep. Generic a rep => ConstrName rep => a -> Tuple String Json
addTag t = tag := constrName t

addContents ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → Json
addContents t = contents := encodeJson t ~> jsonEmptyObject

encodeJsonNewType ∷ ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type). Generic c a ⇒ ConstrName a ⇒ EncodeJson b ⇒ Newtype c b ⇒ c → Json
encodeJsonNewType t = addTag t ~> encodeJson (unwrap t)

bm :: BroadcastMessageFromClient
bm = BroadcastMessageFromClient {contents: "str", recipients: [recip]}

recip :: Recipient
recip = Private usr

usr :: User
usr = User {userName : "ehy"}

show' ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → String
show' t = stringify $ encodeJson t

instance EncodeJson CommandToServer where
  encodeJson p@(Login t) = addTag p ~> addContents t
  encodeJson p@(SendMessage t) = addTag p ~> addContents t
  encodeJson p@(BroadcastMessage t) = addTag p ~> addContents t
  encodeJson p@(CreateChannel t) = addTag p ~> addContents t
  encodeJson p@(RemoveChannel t) = addTag p ~> addContents t
  encodeJson p@(JoinChannel t) = addTag p ~> addContents t
  encodeJson p@(InviteToChannel t) = addTag p ~> addContents t
  encodeJson p@(RemoveFromChannel t) = addTag p ~> addContents t

instance EncodeJson MessageFromClient where
  encodeJson p@(MessageFromClient t) = addTag p ~> encodeJson t

instance EncodeJson BroadcastMessageFromClient where
  encodeJson p@(BroadcastMessageFromClient t) = addTag p ~> encodeJson t

instance EncodeJson Recipient where
  encodeJson p@(Private t) = addTag p ~> addContents t
  encodeJson p@(Public t) = addTag p ~> addContents t

instance EncodeJson Channel where encodeJson = encodeJsonNewType
instance EncodeJson LoginData where encodeJson = encodeJsonNewType
instance EncodeJson ServerNotification where encodeJson = encodeJsonNewType
instance EncodeJson User where encodeJson = encodeJsonNewType

instance Show Channel where show = show'
instance Show LoginData where show = show'
instance Show ServerNotification where show = show'
instance Show User where show = show'
instance Show BroadcastMessageFromClient where show = show'
instance Show CommandToServer where show = show'
instance Show MessageFromClient where show = show'
instance Show Recipient where show = show'






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