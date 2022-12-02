module C_5_3_ASCII_strings where

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

helloRequestString :: BS.ByteString
helloRequestString =
  mapConcat
    [ [A.string|GET /hello.txt HTTP/1.1|]
    , [A.string|User-Agent: curl/7.16.3|]
    , [A.string|User-Agent: curl/7.16.3|]
    ]