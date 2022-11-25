module C_3_4 where

import Data.ByteString as BS (hPut, hPutStr, pack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word8)
import GHC.IO.Handle.FD (stdout)
import System.IO qualified as IO

helloByteString :: IO ()
helloByteString = do
  IO.hSetBinaryMode stdout True
  BS.hPut stdout (BS.pack helloBytes)

helloBytes :: [Word8]
helloBytes =
  [ -- hello
    104
  , 101
  , 108
  , 111
  , -- ,
    32
  , -- world
    119
  , 111
  , 114
  , 108
  , 100
  , 33
  , -- /n
    10
  ]

-- >>>helloByteString

helloUtf8 :: IO ()
helloUtf8 = do
  IO.hSetBinaryMode stdout True
  BS.hPutStr stdout (T.encodeUtf8 (T.pack "hello, world!\n"))

-- >>>helloUtf8
