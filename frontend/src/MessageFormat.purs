module MessageFormat where

-- import Control.Category ((>>>))
-- import Data.Argonaut.Core as J
-- import Data.Codec.Argonaut as CA
-- import Data.Codec.Argonaut.Record as CAR

import Control.Alternative
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Newtype
import Foreign.Object

import Data.Generic.Rep (class Generic, Constructor(..), Product(..), Sum(..), from)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
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
  constrName' (Product a _) = constrName' a

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

-- sp = unwrap (User {userName : ""})

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

addTag :: forall a rep. Generic a rep => ConstrName rep => a -> Tuple String Json
addTag t = tag := constrName t

tag::String
tag = "tag"
contents::String
contents = "contents"

addContents ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → Json
addContents t = contents := encodeJson t ~> jsonEmptyObject

encodeJsonNewType ∷ ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type). Generic c a ⇒ ConstrName a ⇒ EncodeJson b ⇒ Newtype c b ⇒ c → Json
encodeJsonNewType t = addTag t ~> encodeJson (unwrap t)

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
  encodeJson p@(MessageFromClient t) = addTag p ~> addContents t

instance EncodeJson BroadcastMessageFromClient where
  encodeJson p@(BroadcastMessageFromClient t) = addTag p ~> addContents t

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

bm = BroadcastMessageFromClient {contents: "str", recipients: [rec]}

rec = Private usr

usr = User {userName : "ehy"}

-- instance Show MessageFromServer where show = show'
-- instance Show MessageToClient where show = show'
-- instance Show Sender where show = show'

-- for printing constructors
-- class Default a where def :: a

-- instance Default String where def = ""

-- instance Default (Array a) where def = []

-- instance Default BroadcastMessageFromClient where def = BroadcastMessageFromClient {recipients: def, contents: def}
-- instance Default Channel where def = Channel {channelName : def}
-- instance Default LoginData where def = LoginData {userName : def, password: def}
-- instance Default MessageFromClient where def = MessageFromClient {recipient: def, contents: def}
-- instance Default MessageToClient where def = MessageToClient {sender: def, contents: def}
-- instance Default ServerNotification where def = ServerNotification {contents: def}
-- instance Default User where def = User {userName: def}
-- instance Default Recipient where def = Private def
-- instance Default Sender where def = PrivateSender {user : def}


-- instance EncodeJson User where encodeJson = encodeJsonNewType

-- ps = stringify $ encodeJsonNewType $ User {userName : "hey"}

-- ps' = User {userName : "hey"}


-- instance EncodeJson Channel where
--   encodeJson t@(Channel {channelName}) =
--     addTag t
--     ~> "channelName" := channelName
--     ~> jsonEmptyObject


-- codecBroadcastMessageFromClient :: CA.JsonCodec BroadcastMessageFromClient
-- codecBroadcastMessageFromClient = 
--   CA.object (constrName BroadcastMessageFromClient) (
--     CAR.record {
--       recipient : CA.array codecRecipient,
--       contents : CA.string
--     }
--   )

-- codecRecipient :: CA.JsonCodec Recipient
-- codecRecipient = 

-- codecUser :: CA.JsonCodec User
-- codecUser = 
--   let 
--     c = constrName (def :: User)
--   in
--     wrapIso User $
--       CAR.object c ({
--         userName : CA.string
--       })

-- instance encodeJsonAppUser :: EncodeJson AppUser where
--   encodeJson (AppUser { name, age, team }) =
--     "name" := name       -- inserts "name": "Tom"
--       ~> "age" :=? age   -- inserts "age": "25" (if Nothing, does not insert anything)
--       ~>? "team" := team -- inserts "team": "Red Team"
--       ~> jsonEmptyObject

-- ser :: {userName :: String} -> String
-- ser = CA.encode codecUser >>> J.stringify

-- type Person = { name ∷ String, age ∷ Int, active ∷ Boolean }

-- codec ∷ CA.JsonCodec Person
-- codec =
--   CA.object "Person"
--     (CAR.record
--       { name: CA.string
--       , age: CA.int
--       , active: CA.boolean
--       })

-- serP :: Person -> String
-- serP = CA.encode codec >>> J.stringify


-- data P = P
-- derive instance Generic P _
-- instance Show P where show = genericShow

-- type Us = {userName :: String}
