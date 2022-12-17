module AppTypes
  where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut (class EncodeJson, JsonDecodeError(..), jsonEmptyObject, (.:), (:=), (~>))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Array (foldl)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Constructor(..), Product(..), Sum(..), from)
import Data.Map (fromFoldable)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML (HTML, div_)
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

-- TODO read from config
-- the server address (localhost)
-- On GH pages, it should just rewrite the config

-- TODO scrollable list of contacts
-- when press a contact, a chat opens
-- in a chat on the top - arrow back
-- in the bottom - 

