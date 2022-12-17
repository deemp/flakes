{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module TransportTypes
  ( User (..),
    Channel (..),
    Group (..),
    SText (..),
    SAlias (..),
    SInt (..),
    Seconds (..),
    UTC (..),
    SName (..),
    SId (..),
    Entity (..),
    Contents (..),
    MessageFromClient (..),
    Credentials (..),
    AdminPermission (..),
    UserPermission (..),
    Permissions (..),
    Medium (..),
    GroupAction (..),
    MediumAction (..),
    UserAction (..),
    Tuple2 (..),
    Alt2 (..),
    Alt3 (..),
    BroadcastMessage (..),
    MessagesAction (..),
    Request' (..),
    ClientRequest (..),
    Sender (..),
    MessageToClient (..),
    Notification (..),
    GroupActionResponse (..),
    MediumActionResponse (..),
    UserActionResponse (..),
    Reason (..),
    Message' (..),
    Edited (..),
    Response' (..),
    ServerResponse (..),
    Response_ (..),
    Message_ (..),
    Request_ (..),
    User_ (..),
    Group_ (..),
    Channel_ (..),
    Medium_ (..),
    UUser_ (..),
    Entity_ (..),
    Text_ (..),
    Invitation_ (..),
    Bio_ (..),
    Info_ (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Data (Proxy (Proxy))
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.WebSockets (Request (Request))
import TransportTypesTH (options)
import Prelude hiding (Either)

-- Common

-- TODO check message of no more than N characters
-- TODO smart constructor for messages' Internal representation

-- TODO use the same smart constructors on the client's side

newtype User = User {alias :: SAlias User_}
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype Channel = Channel {alias :: SAlias Channel_}
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype Group = Group {alias :: SAlias Group_}
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype SText a = SText {text :: Text}
  deriving (Show, Eq, Ord, Generic)

newtype SAlias a = SAlias {alias :: Text}
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype SInt = SInt {int :: Int}
  deriving (Show, Eq, Ord, Generic, Hashable)

newtype Seconds = Seconds {seconds :: SInt}
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | time
-- e.g. when a message was sent
newtype UTC = UTC {t :: Int}
  deriving (Show, Eq, Ord, Generic)

newtype SName a = SName {name :: Text}
  deriving (Show, Eq, Ord, Generic)

-- | positive integers
-- TODO make a smart constructor
newtype SId a = SId {id :: Int}
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | contents of a message
data Contents
  = CText {textt :: SText Text_}
  | CInvitation {medium :: Medium, texti :: SText Invitation_}
  deriving (Show, Eq, Ord, Generic)

-- | A message possibly to a number of recipients
data MessageFromClient = MessageFromClient
  { recipients :: [Entity],
    contents :: Contents
  }
  deriving (Show, Eq, Ord, Generic)

-- | Login credentials
data Credentials = Credentials
  { alias :: Text,
    password :: Text
  }
  deriving (Show, Eq, Ord, Generic)

data AdminPermission
  = DeleteMessages
  | BanUsers
  | AddUsers
  | AddAdmins
  | InviteUsersViaLink
  deriving (Show, Eq, Ord, Generic)

data UserPermission
  = EditMediumInfo
  | EditMediumName
  | SendMessages
  | PinMessages
  | SendCoarseWords
  | SlowMode Seconds
  deriving (Show, Eq, Ord, Generic)

-- member permissions in a medium
data Permissions = Permissions
  { adminPermissions :: [AdminPermission],
    userPermission :: [UserPermission]
  }
  deriving (Show, Eq, Ord, Generic)

data Medium
  = MChannel {channel :: Channel}
  | MGroup {group :: Group}
  deriving (Show, Eq, Ord, Generic)

-- TODO how to better store permissions?
-- TODO how to deliver messages?
-- TODO ask message ID before it's displayed

data GroupAction
  = -- TODO only in groups
    AskIsGroupMember {user :: User}
  | AskGroupMemberPermissions
  | -- update for all members
    UpdateGroupPermissions
  | UpdateGroupMemberPermissions {user :: User, userPermissions :: [Permissions]}
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

data UserAction
  = AskUserName
  | AskIsOnline
  | AskBio
  | Block
  | ReportSpam
  | DeleteChat
  deriving (Show, Eq, Ord, Generic)

class (ToJSON a, FromJSON a) => IsJSON a

-- Isomorphic to a 2-tuple
data Tuple2 a b = Tuple2 {a2 :: a, b2 :: b}
  deriving (Show, Eq, Ord, Generic)

-- | Isomorphic to Either
data Alt2 a b = A2 {a2 :: a} | B2 {b2 :: b}
  deriving (Show, Eq, Ord, Generic)

data Alt3 a b c = A3 {a3 :: a} | B3 {b3 :: b} | C3 {c3 :: c}
  deriving (Show, Eq, Ord, Generic)

-- TODO limit the number of recipients
-- TODO after sending a message to a user they get into my chat list
data BroadcastMessage = BroadcastMessage
  { recipients :: [Entity],
    contents :: Contents
  }
  deriving (Show, Eq, Ord, Generic)

data MessagesAction
  = Send {contents :: Contents}
  | GetForPeriod {start :: UTC, end :: UTC}
  deriving (Show, Eq, Ord, Generic)

data Request'
  = -- TODO use jwt, e.g.
    -- https://jwt.io/
    Register {credentials :: Credentials}
  | LogIn {credentials :: Credentials}
  | LogOut
  | OnMessages {entities :: [Entity], actions :: MessagesAction}
  | -- client notifies the server that it received the messages
    ReceivedMessages {receivedMessages :: [MessageId], time :: UTC}
  | -- client notifies the server that a user has read the messages
    ReadMessages {readMessages :: [MessageId]}
  | -- client confirms it has received e.g. messages sent in a response
    RespondReceived {responseId :: SId Response_}
  | -- apply "administrative" (not related to messaging) actions to entities
    Apply {args :: [Alt3 (Tuple2 Medium [MediumAction]) (Tuple2 Group [GroupAction]) (Tuple2 User [UserAction])]}
  | -- TODO combine with client-side search
    AskAutoComplete {name :: SName Entity_}
  | AskKnownEntities
  deriving (Show, Eq, Ord, Generic)

-- | a client will determine the relevance of each server response
-- by observing the request ID
-- e.g. when a client doesn't search for a name
-- the auto completion results aren't needed
data ClientRequest = ClientRequest
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
  deriving (Show, Eq, Ord, Generic)

data MessageToClient = MessageToClient
  { sender :: Sender,
    contents :: Contents
  }
  deriving (Show, Eq, Ord, Generic)

-- e.g., when a group is removed

data Notification
  = ChatRemoved {user :: User}
  | MediumRemoved {medium :: Medium}
  | PermissionsChanged {medium :: Medium, permissions :: [Permissions]}
  deriving (Show, Eq, Ord, Generic)

-- TODO
-- permissions updated
-- online are

data GroupActionResponse
  = RespondIsGroupMember {user :: User, isMember :: Bool}
  | RespondGroupMemberPermissions {user :: User, permissions :: [Permissions]}
  | UpdatedGroupPermissions {permissions :: [Permissions]}
  | UpdatedGroupMemberPermissions {user :: User, permissions :: [Permissions]}
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

data UserActionResponse
  = RespondUserName {name :: SName User_}
  | RespondIsOnline {isOnline :: Bool}
  | RespondBio {bio :: SText Bio_}
  | Blocked
  | ReportedSpam
  | DeletedChat
  deriving (Show, Eq, Ord, Generic)

data Reason
  = AllServersAreDown
  | TooMuchEntities
  | TooMuchActions
  | Malformed
  deriving (Show, Eq, Ord, Generic)

data Message' = Message'
  { contents :: Contents,
    messageId :: MessageId
  }
  deriving (Show, Eq, Ord, Generic)

newtype Edited = Edited {edited :: Bool}
  deriving (Show, Eq, Ord, Generic)

data Response'
  = -- TODO more sophisticated
    Registered
  | LoggedIn
  | LoggedOut
  | BannedBy {user :: User}
  | -- TODO maybe not needed as soon as a websocket is open
    -- but maybe a client's response processing queue is full
    AskReceived {responseId :: SId Response_}
  | -- when broadcasting a message, it becomes many messages with different ids
    -- for each entity, they will have a different id
    SentMessagesTo {entities :: [Tuple2 Entity Message']}
  | -- messages from other entities to this client
    GotMessages {messages :: [Tuple2 Message' Edited]}
  | -- read by recipients
    MessagesReadCounts {readCounts :: [Tuple2 Message' SInt]}
  | -- TODO Respond with a list of actual information per medium (and user)
    ApplyComplete
      { args ::
          [ Alt3
              (Tuple2 Medium [MediumActionResponse])
              (Tuple2 Group [GroupActionResponse])
              (Tuple2 User [UserActionResponse])
          ]
      }
  | RespondAutoComplete {name :: SName Entity_, names :: [Tuple2 Entity (SName Entity_)]}
  | InvalidRequest {reason :: Reason}
  | RespondKnownEntities {knownEntities :: [Entity]}
  deriving (Show, Eq, Ord, Generic)

-- | data coming from a server
-- not necessarily a response to a client
data ServerResponse = ServerResponse
  { -- some responses are push notifications
    -- a client didn't request them
    -- so they don't contain a request id
    requestId :: Maybe (SId Request_),
    responseId :: SId Response_,
    response :: Response'
  }
  deriving (Show, Eq, Ord, Generic)

-- TODO safe SId
-- can be used like this

-- f :: TagId a => SId a -> Int
-- f (SId s) = s

class TagId aRequest'

data Response_ = Response_ deriving (TagId)

data Message_ = Message_ deriving (TagId)

data Request_ = Request_ deriving (TagId)

-- some responses are push notifications
class TagAlias a

data User_ = User_ deriving (TagAlias)

data Group_ = Group_ deriving (TagAlias)

data Channel_ = Channel_ deriving (TagAlias)

class TagMedium a

data Medium_ = Medium_ deriving (TagMedium)

class TagUser a

data UUser_ = UUser_ deriving (TagUser)

class TagEntity a

data Entity_ = Entity_ deriving (TagEntity)

class TagContents a

data Text_ = Text_ deriving (TagContents)

data Invitation_ = Invitation_ deriving (TagContents)

class Description a

data Bio_ = Bio_ deriving (Description)

data Info_ = Info_ deriving (Description)

data D = DA | DB

-- class MyShow a where
--   sh :: a -> String
--   default sh :: a -> String
--   sh s = "str"

-- instance MyShow a => Show a where
--   show = sh

-- TODO admin hierarchy

-- Each command should be recognizable

-- {- TODO uncomment
$(deriveJSON options ''SName)

$(deriveJSON options ''SAlias)

$(deriveJSON options ''Channel_)

$(deriveJSON options ''Channel)

$(deriveJSON options ''User_)

$(deriveJSON options ''User)

$(deriveJSON options ''SInt)

$(deriveJSON options ''Seconds)

$(deriveJSON options ''UserPermission)

$(deriveJSON options ''AdminPermission)

$(deriveJSON options ''Permissions)

$(deriveJSON options ''Credentials)

$(deriveJSON options ''SText)

$(deriveJSON options ''Info_)

$(deriveJSON options ''SId)

$(deriveJSON options ''Message_)

$(deriveJSON options ''Medium_)

$(deriveJSON options ''MediumAction)

$(deriveJSON options ''Group_)

$(deriveJSON options ''Group)

$(deriveJSON options ''Entity)

$(deriveJSON options ''Medium)

$(deriveJSON options ''Alt2)

$(deriveJSON options ''Alt3)

$(deriveJSON options ''Sender)

$(deriveJSON options ''Notification)

$(deriveJSON options ''Invitation_)

$(deriveJSON options ''Text_)

$(deriveJSON options ''Contents)

$(deriveJSON options ''MessageFromClient)

$(deriveJSON options ''MessageToClient)

$(deriveJSON options ''UTC)

$(deriveJSON options ''MessagesAction)

$(deriveJSON options ''Tuple2)

$(deriveJSON options ''UserAction)

$(deriveJSON options ''Entity_)

$(deriveJSON options ''GroupAction)

$(deriveJSON options ''Message')

$(deriveJSON options ''Response_)

$(deriveJSON options ''Request')

$(deriveJSON options ''Reason)

$(deriveJSON options ''Bio_)

$(deriveJSON options ''UserActionResponse)

$(deriveJSON options ''GroupActionResponse)

$(deriveJSON options ''MediumActionResponse)

$(deriveJSON options ''Edited)

$(deriveJSON options ''Response')

-- -}