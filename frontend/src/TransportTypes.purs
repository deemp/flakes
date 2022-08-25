module Message where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Class (class DecodeJsonField)
import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJsonWith)
import Data.Argonaut.Types.Generic (Encoding)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)


-- Common

-- TODO check message of no more than N characters
-- TODO smart constructor for messages' Internal representation

-- TODO use the same smart constructors on the client's side

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
  | Left
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

derive instance Generic User _
derive instance Newtype User _
instance EncodeJson User where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson User where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Channel _
derive instance Newtype Channel _
instance EncodeJson Channel where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Channel where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Group _
derive instance Newtype Group _
instance EncodeJson Group where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Group where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (SText a) _
derive instance Newtype (SText a) _
instance EncodeJson (SText a) where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson (SText a) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (SAlias a) _
derive instance Newtype (SAlias a) _
instance EncodeJson (SAlias a) where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson (SAlias a) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic SInt _
derive instance Newtype SInt _
instance EncodeJson SInt where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson SInt where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Seconds _
derive instance Newtype Seconds _
instance EncodeJson Seconds where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Seconds where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic UTC _
derive instance Newtype UTC _
instance EncodeJson UTC where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson UTC where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (SName a) _
derive instance Newtype (SName a) _
instance EncodeJson (SName a) where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson (SName a) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (SId a) _
derive instance Newtype (SId a) _
instance EncodeJson (SId a) where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson (SId a) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Entity _
instance EncodeJson Entity where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Entity where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Contents _
instance EncodeJson Contents where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Contents where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic MessageFromClient _
derive instance Newtype MessageFromClient _
instance EncodeJson MessageFromClient where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson MessageFromClient where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Credentials _
derive instance Newtype Credentials _
instance EncodeJson Credentials where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Credentials where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic AdminPermission _
instance EncodeJson AdminPermission where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson AdminPermission where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic UserPermission _
instance EncodeJson UserPermission where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson UserPermission where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Permissions _
derive instance Newtype Permissions _
instance EncodeJson Permissions where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Permissions where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Medium _
instance EncodeJson Medium where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Medium where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic GroupAction _
instance EncodeJson GroupAction where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson GroupAction where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic MediumAction _
instance EncodeJson MediumAction where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson MediumAction where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic UserAction _
instance EncodeJson UserAction where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson UserAction where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (Tuple2 a b) _
derive instance Newtype (Tuple2 a b) _
instance (EncodeJson a, EncodeJson b) => EncodeJson (Tuple2 a b) where encodeJson a = genericEncodeJsonWith enc a
instance (DecodeJsonField a, DecodeJsonField b) => DecodeJson (Tuple2 a b) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (Alt2 a b) _
instance (EncodeJson a, EncodeJson b) => EncodeJson (Alt2 a b) where encodeJson a = genericEncodeJsonWith enc a
instance (DecodeJsonField a, DecodeJsonField b) => DecodeJson (Alt2 a b) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic (Alt3 a b c) _
instance (EncodeJson a, EncodeJson b, EncodeJson c) => EncodeJson (Alt3 a b c) where encodeJson a = genericEncodeJsonWith enc a
instance (DecodeJsonField a, DecodeJsonField b, DecodeJsonField c) => DecodeJson (Alt3 a b c) where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic BroadcastMessage _
derive instance Newtype BroadcastMessage _
instance EncodeJson BroadcastMessage where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson BroadcastMessage where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic MessagesAction _
instance EncodeJson MessagesAction where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson MessagesAction where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Request' _
instance EncodeJson Request' where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Request' where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic ClientRequest _
derive instance Newtype ClientRequest _
instance EncodeJson ClientRequest where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson ClientRequest where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Sender _
instance EncodeJson Sender where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Sender where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic MessageToClient _
derive instance Newtype MessageToClient _
instance EncodeJson MessageToClient where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson MessageToClient where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Notification _
instance EncodeJson Notification where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Notification where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic GroupActionResponse _
instance EncodeJson GroupActionResponse where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson GroupActionResponse where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic MediumActionResponse _
instance EncodeJson MediumActionResponse where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson MediumActionResponse where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic UserActionResponse _
instance EncodeJson UserActionResponse where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson UserActionResponse where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Reason _
instance EncodeJson Reason where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Reason where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Message' _
derive instance Newtype Message' _
instance EncodeJson Message' where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Message' where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Edited _
derive instance Newtype Edited _
instance EncodeJson Edited where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Edited where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Response' _
instance EncodeJson Response' where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Response' where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic ServerResponse _
derive instance Newtype ServerResponse _
instance EncodeJson ServerResponse where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson ServerResponse where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Response_ _
instance EncodeJson Response_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Response_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Message_ _
instance EncodeJson Message_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Message_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Request_ _
instance EncodeJson Request_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Request_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic User_ _
instance EncodeJson User_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson User_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Group_ _
instance EncodeJson Group_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Group_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Channel_ _
instance EncodeJson Channel_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Channel_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Medium_ _
instance EncodeJson Medium_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Medium_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic UUser_ _
instance EncodeJson UUser_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson UUser_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Entity_ _
instance EncodeJson Entity_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Entity_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Text_ _
instance EncodeJson Text_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Text_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Invitation_ _
instance EncodeJson Invitation_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Invitation_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Bio_ _
instance EncodeJson Bio_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Bio_ where decodeJson a = genericDecodeJsonWith enc a
derive instance Generic Info_ _
instance EncodeJson Info_ where encodeJson a = genericEncodeJsonWith enc a
instance DecodeJson Info_ where decodeJson a = genericDecodeJsonWith enc a

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
