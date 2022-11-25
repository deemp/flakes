module C_3_6 where

import Data.ByteString as BS (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T

greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
  Left _ -> putStrLn "Invalid byte string"
  Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)
