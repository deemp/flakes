module C_9_Content_types (textOk, countHelloText, countHelloHtml, htmlOk) where

import ASCII qualified as A
import C_6_HTTP_types (
  FieldValue (FieldValue),
  HeaderField (HeaderField),
  MessageBody (MessageBody),
  Request,
  Response (..),
 )
import C_7_Encoding (encodeRequest, encodeResponse)
import C_8_Responding (
  contentLengthField,
  contentType,
  ok,
  sendResponse,
  status,
 )
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J.Key
import Data.Aeson.KeyMap qualified as J.KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TL
import Data.Text.Lazy.Builder.Int qualified as TL
import Data.Text.Lazy.Encoding qualified as LT (encodeUtf8)
import GHC.Natural (Natural)
import Network.Simple.TCP (HostPreference (..), serve)
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 qualified as BR
import Text.Blaze.Html5 as Html ()
import Text.Blaze.Html5 qualified as HTML

-- 9.1 Some common types

plainUtf8 :: FieldValue
plainUtf8 = FieldValue [A.string|text/plain; charset=utf-8|]

htmlUtf8 :: FieldValue
htmlUtf8 = FieldValue [A.string|text/html; charset=utf-8|]

json :: FieldValue
json = FieldValue [A.string|application/json|]

-- 9.2 UTF-8

countHelloText :: Natural -> LT.Text
countHelloText count =
  TL.toLazyText $
    TL.fromString "Hello! \9835\r\n"
      <> case count of
        0 -> TL.fromString "This page has never been viewed."
        1 -> TL.fromString "This page has never been viewed 1 time."
        _ -> TL.fromString "This page has been viewed " <> TL.decimal count <> TL.fromString " times."

helloNote :: LT.Text
helloNote = countHelloText 3

textOk :: LT.Text -> Response
textOk str = Response (status ok) [typ, len] (Just body)
 where
  typ = HeaderField contentType plainUtf8
  len = contentLengthField body
  -- should convert text to bytestring
  body = MessageBody (LT.encodeUtf8 str)

stuckCountingServerText :: IO a
stuckCountingServerText = serve @IO HostAny "8000" \(s, _) -> do
  let count = 0
  sendResponse s (textOk (countHelloText count))

-- 9.3 HTML

countHelloHtml :: Natural -> Html
countHelloHtml count = HTML.docType <> htmlDocument
 where
  htmlDocument =
    HTML.html $
      documentMetadata <> documentBody
  documentMetadata = HTML.head titleHtml
  titleHtml = HTML.title (toHtml "My great web page")
  documentBody =
    HTML.body $
      greetingHtml <> HTML.hr <> hitCounterHtml
  greetingHtml = HTML.p (toHtml "Hello! \9835")
  hitCounterHtml = HTML.p $ case count of
    0 -> toHtml "This page has never been viewed."
    1 -> toHtml "This page has been viewed 1 time."
    _ ->
      toHtml "This page has been viewed "
        <> toHtml @Natural count
        <> toHtml " times."

renderHtml' :: Html -> LBS.ByteString
renderHtml' = BR.renderHtml

-- 9.4 JSON

countHelloJSON1 :: Natural -> J.Value
countHelloJSON1 count = toJSON (J.KeyMap.fromList [greetingJson, hitsJson])
 where
  greetingJson = (J.Key.fromString "greeting", toJSON "Hello! \9835")
  hitsJson = (J.Key.fromString "hits", toJSON (J.KeyMap.fromList [numberJson, messageJson]))
  numberJson = (J.Key.fromString "count", toJSON count)
  messageJson = (J.Key.fromString "message", toJSON (countHelloText count))

ch :: J.Value
ch = countHelloJSON1 3

{-
>>>ch
Object (fromList [("greeting",String "Hello! \9835"),("hits",Object (fromList [("count",Number 3.0),("message",String "Hello! \9835\r\nThis page has been viewed 3 times.")]))])
-}

countHelloJSON :: Natural -> J.Value
countHelloJSON count =
  J.object
    [ fromString "greeting" .= fromString @T.Text "Hello! \9835"
    , fromString "hits"
        .= J.object
          [ fromString "count" .= count
          , fromString "message" .= countHelloText count
          ]
    ]

jsonOk :: J.Value -> Response
jsonOk str = Response (status ok) [typ, len] (Just body)
 where
  typ = HeaderField contentType json
  len = contentLengthField body
  body = MessageBody (J.encode str)

-- 9.5 Exercises

htmlOk :: Html -> Response
htmlOk str = Response (status ok) [typ, len] (Just body)
 where
  typ = HeaderField contentType htmlUtf8
  len = contentLengthField body
  body = MessageBody (BR.renderHtml str)

stuckCountingServerHtml :: IO a
stuckCountingServerHtml = serve @IO HostAny "8000" \(s, _) -> do
  let count = 0
  sendResponse s (htmlOk (countHelloHtml count))

--- Ex 25

class Encode a where
  encode :: a -> BSB.Builder

instance Encode Request where
  encode :: Request -> BSB.Builder
  encode = encodeRequest

instance Encode Response where
  encode :: Response -> BSB.Builder
  encode = encodeResponse

instance Encode Integer where
  encode :: Integer -> BSB.Builder
  encode = BSB.integerDec

instance Encode Int64 where
  encode :: Int64 -> BSB.Builder
  encode = BSB.int64Dec

instance Encode T.Text where
  encode :: T.Text -> BSB.Builder
  encode = BSB.lazyByteString . LT.encodeUtf8 . LT.fromStrict

instance Encode LT.Text where
  encode :: LT.Text -> BSB.Builder
  encode = BSB.lazyByteString . LT.encodeUtf8

instance Encode BS.ByteString where
  encode :: BS.ByteString -> BSB.Builder
  encode = BSB.byteString

instance Encode LBS.ByteString where
  encode :: LBS.ByteString -> BSB.Builder
  encode = BSB.lazyByteString

instance Encode BSB.Builder where
  encode :: BSB.Builder -> BSB.Builder
  encode = id
