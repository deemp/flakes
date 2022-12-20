module C_7_Encoding (
  encodeLineEnd,
  encodeStatusLine,
  encodeResponse,
  encodeRequestLine,
  repeatedlyEncode,
  encodeHeaderField,
  encodeMessageBody,
  optionallyEncode,
  encodeRequest,
) where

import ASCII qualified as A
import ASCII.Char qualified as AC
import C_1_Handles (getDataDir)
import C_5_HTTP (crlf, helloRequestString, helloResponseString)
import C_6_HTTP_types (
  FieldName (FieldName),
  FieldValue (FieldValue),
  HeaderField (HeaderField),
  HttpVersion (..),
  MessageBody (MessageBody),
  Method (Method),
  ReasonPhrase (ReasonPhrase),
  Request (Request),
  RequestLine (RequestLine),
  RequestTarget (RequestTarget),
  Response (Response),
  StatusCode (StatusCode),
  StatusLine (StatusLine),
  helloRequest,
  helloResponse,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Foldable (Foldable (..))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as TB
import Data.Time qualified as Time
import System.FilePath ((</>))

-- 7.1 String builders

sayHello :: T.Text -> T.Text
sayHello name = T.pack "Hello, " <> name <> T.pack "!"

{-
>>>sayHello $ T.pack "Tim"
"Hello, Tim!"
-}

sayHelloWithBuilder :: T.Text -> T.Text
sayHelloWithBuilder name =
  LT.toStrict $
    TB.toLazyText $
      TB.fromString "Hello" <> TB.fromText name <> TB.fromString "!"

-- 7.2 Measuring time

time :: IO () -> IO ()
time action = do
  a <- Time.getCurrentTime
  action
  b <- Time.getCurrentTime
  print (Time.diffUTCTime b a)

concatWithStrict :: Int -> T.Text
concatWithStrict n = fold $ replicate n $ T.pack "a"

concatWithBuilder :: Int -> T.Text
concatWithBuilder n = LT.toStrict $ TB.toLazyText $ fold $ replicate n $ TB.fromString "a"

concatSpeedTest :: Int -> IO ()
concatSpeedTest n = do
  dir <- getDataDir
  time $ T.writeFile (dir </> "builder.txt") (concatWithBuilder n)
  time $ T.writeFile (dir </> "strict.txt") (concatWithStrict n)

{-
>>>concatSpeedTest 10000
-}

-- 7.3 Request and response

encodeRequest :: Request -> BSB.Builder
encodeRequest (Request requestLine headerFields bodyMaybe) =
  encodeRequestLine requestLine
    <> repeatedlyEncode (\x -> encodeHeaderField x <> encodeLineEnd) headerFields
    <> encodeLineEnd
    <> optionallyEncode encodeMessageBody bodyMaybe

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine headerFields bodyMaybe) =
  encodeStatusLine statusLine
    <> repeatedlyEncode (\x -> encodeHeaderField x <> encodeLineEnd) headerFields
    <> encodeLineEnd
    <> optionallyEncode encodeMessageBody bodyMaybe

encodeLineEnd :: BSB.Builder
encodeLineEnd = A.lift crlf

-- 7.4 Higher-order encodings

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode = foldMap

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode = foldMap

-- 7.5 The start line

encodeSpace :: BSB.Builder
encodeSpace = A.lift [AC.Space]

encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method requestTarget httpVersion) =
  encodeMethod method
    <> encodeSpace
    <> encodeRequestTarget requestTarget
    <> encodeSpace
    <> encodeHttpVersion httpVersion
    <> encodeLineEnd

encodeMethod :: Method -> BSB.Builder
encodeMethod (Method m) = BSB.byteString m

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget rt) = BSB.byteString rt

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine httpVersion statusCode reasonPhrase) =
  encodeHttpVersion httpVersion
    <> encodeSpace
    <> encodeStatusCode statusCode
    <> encodeSpace
    <> encodeReasonPhrase reasonPhrase
    <> encodeLineEnd

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode c1 c2 c3) = A.lift [c1, c2, c3]

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase s) = BSB.byteString s

encodeHttpVersion :: HttpVersion -> BSB.Builder
encodeHttpVersion (HttpVersion v1 v2) =
  BSB.byteString [A.string|HTTP/|]
    <> A.digitString v1
    <> A.lift [AC.FullStop]
    <> A.digitString v2

-- 7.6 Exercises

encodeHeaderField :: HeaderField -> BSB.Builder
encodeHeaderField (HeaderField (FieldName x) (FieldValue y)) =
  BSB.byteString x
    <> A.lift [AC.Colon]
    <> encodeSpace
    <> BSB.byteString y

encodeMessageBody :: MessageBody -> BSB.Builder
encodeMessageBody (MessageBody s) = BSB.lazyByteString s

req :: BSB.Builder
req = encodeRequest helloRequest

{-
>>>req
"GET /hello.txt HTTP/1.1\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"
-}

resp :: BSB.Builder
resp = encodeResponse helloResponse

{-
>>>resp
"HTTP/1.1 200 OK\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\nHello"
-}

requestEqual :: Bool
requestEqual = BS.toStrict (BSB.toLazyByteString req) == helloRequestString

{-
>>>reqEqual
True
>>>helloRequestString
"GET /hello.txt HTTP/1.1\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"
>>>req
"GET /hello.txt HTTP/1.1\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"
-}

responseEqual :: Bool
responseEqual = BS.toStrict (BSB.toLazyByteString resp) == helloResponseString

{-
>>>responseEqual
True
>>>helloResponseString
"HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=us-ascii\r\nContent-Length: 6\r\n\r\nHello"
>>>resp
"HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=us-ascii\r\nContent-Length: 6\r\n\r\nHello"
-}
