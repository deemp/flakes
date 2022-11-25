module C_3_6_Exercises where

import Data.ByteString as BS (ByteString, map)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.String (fromString)

-- Ex 8

greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
  Left _ -> putStrLn "Invalid byte string"
  Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)

p1 = T.decodeUtf8' "♫"

-- >>>p1
-- Right "k"

-- Ex 9
asciiUpper :: BS.ByteString -> BS.ByteString
asciiUpper = BS.map (\x -> if 97 <= x && x <= 122 then 65 + x - 97 else x)

p2 :: ByteString
p2 = asciiUpper (fromString "Hello!")

-- >>> p2
-- "HELLO!"
