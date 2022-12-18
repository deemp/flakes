module TransportTypes where

import Data.Argonaut (Json, JsonDecodeError, stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (class DecodeRep, genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeRep, genericEncodeJsonWith)
import Data.Argonaut.Types.Generic (Encoding)
import Data.Codec.Argonaut.Common (either)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Class.Console (logShow)


-- Common

-- TODO check message of no more than N characters
-- TODO smart constructor for messages' Internal representation

-- TODO use the same smart constructors on the client's side

-- TODO what message data should be stored along its id on the client?
-- sender alias
-- time
-- which message it is a response to
-- how many messages respond this one

-- TODO show when a message was received

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


newtype User = User {alias :: SAlias User_}

newtype Channel = Channel {alias :: SAlias Channel_}

newtype Group = Group {alias :: SAlias Group_}

newtype SText :: forall k. k -> Type
newtype SText a = SText {text :: String}

newtype SAlias :: forall k. k -> Type
newtype SAlias a = SAlias {alias :: String}

newtype SInt = SInt {int :: Int}

newtype Seconds = Seconds {seconds :: SInt}

-- | time
-- e.g. when a message was sent
newtype UTC = UTC {t :: Int}

newtype SName :: forall k. k -> Type
newtype SName a = SName {name :: String}

-- | positive integers
-- TODO make a smart constructor
newtype SId :: forall k. k -> Type
newtype SId a = SId {id :: Int}

type MessageId = SId Message_

-- Client -> Server

-- When a client sends a message, server knows its alias
-- So this client only specifies the entity

-- A client can only send messages by an alias

-- | Each entity has a unique immutable alias
-- This alias is used to send messages
data Entity
  = Group' {group :: Group}
  | Channel' {channel :: Channel}
  | User' {user :: User}

-- | contents of a message
data Contents
  = CText {textt :: SText Text_}
  | CInvitation {medium :: Medium, texti :: SText Invitation_}

-- | A message possibly to a number of recipients
newtype MessageFromClient = MessageFromClient
  { recipients :: Array Entity,
    contents :: Contents
  }

-- | Login credentials
newtype Credentials = Credentials
  { alias :: String,
    password :: String
  }

data AdminPermission
  = DeleteMessages
  | BanUsers
  | AddUsers
  | AddAdmins
  | InviteUsersViaLink

data UserPermission
  = EditMediumInfo
  | EditMediumName
  | SendMessages
  | PinMessages
  | SendCoarseWords
  | SlowMode Seconds

-- member permissions in a medium
newtype Permissions = Permissions
  { adminPermissions :: Array AdminPermission,
    userPermission :: Array UserPermission
  }

data Medium
  = MChannel {channel :: Channel}
  | MGroup {group :: Group}

-- TODO how to better store permissions?
-- TODO how to deliver messages?
-- TODO ask message ID before it's displayed

data GroupAction
  = -- TODO only in groups
    AskIsGroupMember {user :: User}
  | AskGroupMemberPermissions
  | -- update for all members
    UpdateGroupPermissions
  | UpdateGroupMemberPermissions {user :: User, userPermissions :: Array Permissions}

-- | All possible actions applicable to a medium
data MediumAction
  = Create
  | Leave
  | Join
  | Delete
  | AskInfo
  | AskName
  | EditName {newName :: SName Medium_}
  | EditInfo {newInfo :: SText Info_}
  | DeleteMessage {messageId :: MessageId}
  | PinMessage {messageId :: MessageId}
  | ReplyMessage {originalMessage :: MessageId, replyMessage :: MessageId}
  | Ban {user :: User}
  | Add {user :: User}

data UserAction
  = AskUserName
  | AskIsOnline
  | AskBio
  | Block
  | ReportSpam
  | DeleteChat

class (EncodeJson a, DecodeJson a) <= IsJSON a

-- Isomorphic to a 2-tuple
newtype Tuple2 a b = Tuple2 {a2 :: a, b2 :: b}

-- | Isomorphic to Either
data Alt2 a b = A2 {a2 :: a} | B2 {b2 :: b}

data Alt3 a b c = A3 {a3 :: a} | B3 {b3 :: b} | C3 {c3 :: c}

-- TODO limit the number of recipients
-- TODO after sending a message to a user they get into my chat list
newtype BroadcastMessage = BroadcastMessage
  { recipients :: Array Entity,
    contents :: Contents
  }

data MessagesAction
  = Send {contents :: Contents}
  | GetForPeriod {start :: UTC, end :: UTC}

data Request'
  = -- TODO use jwt, e.g.
    -- https://jwt.io/
    Register {credentials :: Credentials}
  | LogIn {credentials :: Credentials}
  | LogOut
  | OnMessages {entities :: Array Entity, actions :: MessagesAction}
  | -- client notifies the server that it received the messages
    ReceivedMessages {receivedMessages :: Array MessageId, time :: UTC}
  | -- client notifies the server that a user has read the messages
    ReadMessages {readMessages :: Array MessageId}
  | -- client confirms it has received e.g. messages sent in a response
    RespondReceived {responseId :: SId Response_}
  | -- apply "administrative" (not related to messaging) actions to entities
    Apply {args :: Array (Alt3 (Tuple2 Medium (Array MediumAction)) (Tuple2 Group (Array GroupAction)) (Tuple2 User (Array UserAction)))}
  | -- TODO combine with client-side search
    AskAutoComplete {name :: SName Entity_}
  | -- get a list of known entities
    AskKnownEntities
    

-- | a client will determine the relevance of each server response
-- by observing the request ID
-- e.g. when a client doesn't search for a name
-- the auto completion results aren't needed
newtype ClientRequest = ClientRequest
  { -- if the server sent you some messages it may ask if you've read them
    -- if it doesn't get a response it will repeat to ask
    -- with the same response (meaning the request coming from the server) id
    responseId :: Maybe (SId Response_),
    --  a client enumerates its requests so that a server can relate its responses to them
    requestId :: SId Request_,
    request :: Request'
  }

-- Server -> Client

-- | When a server sends a message to its client
-- * in public media, a sender's name can be hidden
-- * in private chats, a sender's alias is always known
data Sender
  = PublicSender {medium :: Medium, sender :: Alt2 User (SName User_)}
  | PrivateSender {user :: User}

newtype MessageToClient = MessageToClient
  { sender :: Sender,
    contents :: Contents
  }

-- f :: forall a. IsJSON a => a -> Int
-- f s = 3
-- e.g., when a group is removed

data Notification
  = ChatRemoved {user :: User}
  | MediumRemoved {medium :: Medium}
  | PermissionsChanged {medium :: Medium, permissions :: Array Permissions}

-- TODO
-- permissions updated
-- online are

data GroupActionResponse
  = RespondIsGroupMember {user :: User, isMember :: Boolean}
  | RespondGroupMemberPermissions {user :: User, permissions :: Array Permissions}
  | UpdatedGroupPermissions {permissions :: Array Permissions}
  | UpdatedGroupMemberPermissions {user :: User, permissions :: Array Permissions}

data MediumActionResponse
  = Created
  | LeftMedium
  | Joined
  | Deleted
  | Info {info :: SText Info_}
  | RespondName {name :: SName Medium_}
  | EditedName {name :: SName Medium_}
  | EditedInfo {info :: SText Info_}
  | DeletedMessage {messageId :: MessageId}
  | PinnedMessage {messageId :: MessageId}
  | RepliedMessage {originalMessage :: MessageId, replyMessage :: MessageId}
  | Banned {user :: User}
  | Added {user :: User}

data UserActionResponse
  = RespondUserName {name :: SName User_}
  | RespondIsOnline {isOnline :: Boolean}
  | RespondBio {bio :: SText Bio_}
  | Blocked
  | ReportedSpam
  | DeletedChat

data Reason
  = AllServersAreDown
  | TooMuchEntities
  | TooMuchActions
  | Malformed

newtype Message' = Message'
  { contents :: Contents,
    messageId :: MessageId
  }

newtype Edited = Edited {edited :: Boolean}

data Response'
  = -- TODO more sophisticated
    Registered
  | LoggedIn
  | LoggedOut
  | BannedBy {user :: User}
  | AskReceived {responseId :: SId Response_}
  | -- when broadcasting a message, it becomes many messages with different ids
    -- for each entity, they will have a different id
    SentMessagesTo {entities :: Array (Tuple2 Entity Message')}
  | -- messages from other entities to this client
    GotMessages {messages :: Array (Tuple2 Message' Edited)}
  | -- read by recipients
    MessagesReadCounts {readCounts :: Array (Tuple2 Message' SInt)}
  | -- TODO Respond with a list of actual information per medium (and user)
    ApplyComplete
      { args :: Array
          ( Alt3
              (Tuple2 Medium (Array MediumActionResponse))
              (Tuple2 Group (Array GroupActionResponse))
              (Tuple2 User (Array UserActionResponse))
          )
      }
  | RespondAutoComplete {name :: SName Entity_, names :: Array (Tuple2 Entity (SName Entity_))}
  | InvalidRequest {reason :: Reason}
  | RespondKnownEntities {knownEntities :: Array Entity}


-- | data coming from a server
-- not necessarily a response to a client
newtype ServerResponse = ServerResponse
  { -- some responses are push notifications
    -- a client didn't request them
    -- so they don't contain a request id
    requestId :: Maybe (SId Request_),
    responseId :: SId Response_,
    response :: Response'
  }

-- TODO safe SId
-- can be used like this

-- f :: TagId a => SId a -> Int
-- f (SId s) = s

class TagId :: forall k. k -> Constraint
class TagId aRequest'

data Response_ = Response_
instance TagId Response_

data Message_ = Message_
instance TagId Message_

data Request_ = Request_
instance TagId Request_

-- some responses are push notifications
class TagAlias :: forall k. k -> Constraint
class TagAlias a

data User_ = User_
instance TagAlias User_

data Group_ = Group_
instance TagAlias Group_

data Channel_ = Channel_
instance TagAlias Channel_

class TagMedium :: forall k. k -> Constraint
class TagMedium a

data Medium_ = Medium_
instance TagMedium Medium_

class TagUser :: forall k. k -> Constraint
class TagUser a

data UUser_ = UUser_
instance TagUser UUser_

class TagEntity :: forall k. k -> Constraint
class TagEntity a

data Entity_ = Entity_
instance TagEntity Entity_

class TagContents :: forall k. k -> Constraint
class TagContents a

data Text_ = Text_
instance TagContents Text_

data Invitation_ = Invitation_
instance TagContents Invitation_

class Description :: forall k. k -> Constraint
class Description a

data Bio_ = Bio_
instance Description Bio_

data Info_ = Info_
instance Description Info_


tag::String
tag = "(tag)"

contents::String
contents = "(contents)"

enc :: Encoding
enc = {tagKey: tag, unwrapSingleArguments: true, valuesKey: Nothing}

-- {- 
derive instance Generic User _
derive instance Newtype User _
derive instance Generic Channel _
derive instance Newtype Channel _
derive instance Generic Group _
derive instance Newtype Group _
derive instance Generic (SText a) _
derive instance Newtype (SText a) _
derive instance Generic (SAlias a) _
derive instance Newtype (SAlias a) _
derive instance Generic SInt _
derive instance Newtype SInt _
derive instance Generic Seconds _
derive instance Newtype Seconds _
derive instance Generic UTC _
derive instance Newtype UTC _
derive instance Generic (SName a) _
derive instance Newtype (SName a) _
derive instance Generic (SId a) _
derive instance Newtype (SId a) _
derive instance Generic Entity _
derive instance Generic Contents _
derive instance Generic MessageFromClient _
derive instance Newtype MessageFromClient _
derive instance Generic Credentials _
derive instance Newtype Credentials _
derive instance Generic AdminPermission _
derive instance Generic UserPermission _
derive instance Generic Permissions _
derive instance Newtype Permissions _
derive instance Generic Medium _
derive instance Generic GroupAction _
derive instance Generic MediumAction _
derive instance Generic UserAction _
derive instance Generic (Tuple2 a b) _
derive instance Newtype (Tuple2 a b) _
derive instance Generic (Alt2 a b) _
derive instance Generic (Alt3 a b c) _
derive instance Generic BroadcastMessage _
derive instance Newtype BroadcastMessage _
derive instance Generic MessagesAction _
derive instance Generic Request' _
derive instance Generic ClientRequest _
derive instance Newtype ClientRequest _
derive instance Generic Sender _
derive instance Generic MessageToClient _
derive instance Newtype MessageToClient _
derive instance Generic Notification _
derive instance Generic GroupActionResponse _
derive instance Generic MediumActionResponse _
derive instance Generic UserActionResponse _
derive instance Generic Reason _
derive instance Generic Message' _
derive instance Newtype Message' _
derive instance Generic Edited _
derive instance Newtype Edited _
derive instance Generic Response' _
derive instance Generic ServerResponse _
derive instance Newtype ServerResponse _
derive instance Generic Response_ _
derive instance Generic Message_ _
derive instance Generic Request_ _
derive instance Generic User_ _
derive instance Generic Group_ _
derive instance Generic Channel_ _
derive instance Generic Medium_ _
derive instance Generic UUser_ _
derive instance Generic Entity_ _
derive instance Generic Text_ _
derive instance Generic Invitation_ _
derive instance Generic Bio_ _
derive instance Generic Info_ _
-- -}


class MyJson a where
  myEncodeJson :: a -> Json
  myDecodeJson :: Json -> Either JsonDecodeError a

newtype MyJ a = MyJ a

t = MyJ

instance (Generic a b, EncodeRep b, DecodeRep b) => MyJson a where 
  myEncodeJson = genericEncodeJsonWith enc
  myDecodeJson = genericDecodeJsonWith enc

instance (MyJson a) => EncodeJson (MyJ a) where encodeJson (MyJ a) = myEncodeJson a
instance (MyJson a) => DecodeJson (MyJ a) where 
  decodeJson j = 
    case myDecodeJson j of
      Left err -> Left err
      Right a -> Right $ MyJ a

pt ∷ String
pt = stringify $ encodeJson $ MyJ (SAlias {alias : "usr"} :: SAlias Group_)