module C_5_4_HTTP_responses where

import ASCII qualified as A
import ASCII.Char qualified as A
import Data.ByteString (intercalate)
import Data.ByteString qualified as BS

line :: BS.ByteString -> BS.ByteString
line x = x <> A.lift crlf

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

text :: [BS.ByteString] -> BS.ByteString
text xs = intercalate "" (map line xs)

mapConcat :: [BS.ByteString] -> BS.ByteString
mapConcat x = intercalate "" $ map line x

helloResponseString :: BS.ByteString
helloResponseString =
  mapConcat
    [ [A.string|HTTP/1.1 200 OK|]
    , [A.string|Content-Type: text/plain; charset=us-ascii|]  
    , [A.string|Content-Length: 6|]
    , [A.string||]
    ]
    <> [A.string|Hello|]