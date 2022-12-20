module C_8_Responding (
  contentLength,
  contentType,
  status,
  ok,
  bodyLengthValue,
  contentLengthField,
  sendResponse,
) where

import ASCII qualified as A
import ASCII.Decimal qualified as A
import C_6_HTTP_types (
  FieldName (FieldName),
  FieldValue (FieldValue),
  HeaderField (HeaderField),
  HttpVersion (HttpVersion),
  MessageBody (MessageBody),
  ReasonPhrase (..),
  Response (Response),
  StatusCode (..),
  StatusLine (..),
 )
import C_7_Encoding (encodeLineEnd, encodeResponse, encodeStatusLine)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)
import GHC.Natural (Natural)
import Network.Simple.TCP (HostPreference (HostAny), Socket, serve)
import Network.Simple.TCP qualified as Net

-- 8.1 A measure of success

countHelloAscii :: Natural -> LBS.ByteString
countHelloAscii count =
  BSB.toLazyByteString $
    [A.string|Hello!|]
      <> encodeLineEnd
      <> [A.string|This page has |]
      <> case count of
        0 -> [A.string|never been viewed.|]
        1 -> [A.string|been viewed 1 time.|]
        _ ->
          [A.string|been viewed |]
            <> A.showIntegralDecimal count
            <> [A.string| times.|]

data Status = Status StatusCode ReasonPhrase

-- 8.2 Response-building utilities

ok :: Status
ok =
  Status
    (StatusCode A.Digit2 A.Digit0 A.Digit0)
    (ReasonPhrase [A.string|OK|])

status :: Status -> StatusLine
status (Status statusCode reasonPhrase) = StatusLine http_1_1 statusCode reasonPhrase

http_1_1 :: HttpVersion
http_1_1 = HttpVersion A.Digit1 A.Digit1

encOk :: LBS.ByteString
encOk = BSB.toLazyByteString (encodeStatusLine (status ok))

{-
>>>encOk
"HTTP/1.1 200 OK\r\n"
-}

contentType :: FieldName
contentType = FieldName [A.string|Content-Type|]

contentLength :: FieldName
contentLength = FieldName [A.string|Content-Length|]

contentLengthField :: MessageBody -> HeaderField
contentLengthField body = HeaderField contentLength (bodyLengthValue body)

plainAscii :: FieldValue
plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]

asciiOk :: LBS.ByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
 where
  typ = HeaderField contentType plainAscii
  len = HeaderField contentLength (bodyLengthValue body)
  body = MessageBody str

-- this is for Content-Length
bodyLengthValue :: MessageBody -> FieldValue
bodyLengthValue (MessageBody x) = FieldValue (A.showIntegralDecimal (LBS.length x))

-- 8.3 Integers

-- 8.4 Response transmission

sendResponse :: Socket -> Response -> IO ()
sendResponse s r =
  Net.sendLazy s $
    BSB.toLazyByteString (encodeResponse r)

stuckCountingServer :: IO a
stuckCountingServer = serve @IO HostAny "8000" \(s, _) -> do
  let count = 0
  sendResponse s (asciiOk (countHelloAscii count))

-- 8.5 Exercises

--- Ex 21

-- c`url http://localhost:8000 --dump-header -

--- Ex 22

mid :: Word8 -> Word8 -> Word8
mid x y = fromInteger (div (toInteger x + toInteger y) 2)

{-
>>>mid 3 5
4
>>>mid 220 250
235
-}
