module C_6_HTTP_types where

import ASCII qualified as A
import ASCII.Decimal qualified as A (Digit (..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (toChunks)
import Data.ByteString.Lazy qualified as LBS

data Request = Request RequestLine [HeaderField] (Maybe MessageBody)
data Response = Response StatusLine [HeaderField] (Maybe MessageBody)
data RequestLine = RequestLine Method RequestTarget HttpVersion
data StatusLine = StatusLine HttpVersion StatusCode ReasonPhrase
data StatusCode = StatusCode A.Digit A.Digit A.Digit
data HeaderField = HeaderField FieldName FieldValue
data HttpVersion = HttpVersion A.Digit A.Digit
newtype Method = Method BS.ByteString
newtype RequestTarget = RequestTarget BS.ByteString
newtype ReasonPhrase = ReasonPhrase BS.ByteString
newtype FieldName = FieldName BS.ByteString
newtype FieldValue = FieldValue BS.ByteString
newtype MessageBody = MessageBody LBS.ByteString

-- 6.7 Exercises

--- Ex 16

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
 where
  start = RequestLine (Method [A.string|GET|]) (RequestTarget [A.string|/hello.txt|]) (HttpVersion A.Digit1 A.Digit1)
  host = HeaderField (FieldName [A.string|Host|]) (FieldValue [A.string|www.example.com|])
  lang = HeaderField (FieldName [A.string|Accept-Language|]) (FieldValue [A.string|en, mi|])

helloResponse :: Response
helloResponse = Response start [host, lang] (Just $ MessageBody [A.string|Hello|])
 where
  start = StatusLine (HttpVersion A.Digit1 A.Digit1) (StatusCode A.Digit2 A.Digit0 A.Digit0) (ReasonPhrase [A.string|OK|])
  host = HeaderField (FieldName [A.string|Content-Type|]) (FieldValue [A.string|text/plain; charset=us-ascii|])
  lang = HeaderField (FieldName [A.string|Content-Length|]) (FieldValue [A.string|6|])

--- Ex 17

inf :: LBS.ByteString
inf = [A.string|abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefgh|]

p :: [Int]
p = BS.length <$> toChunks inf

{-
>>>p
[32,64,128,256,512,384]

>>>LBS.take 10 inf
"abcdefghij"
-}

httpVersion :: HttpVersion
httpVersion = HttpVersion A.Digit1 A.Digit1