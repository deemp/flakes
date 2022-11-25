module C_3_1 where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString as BS
import Data.Word (Word8)
import System.IO qualified as IO

-- Bytes

-- Packed octet
-- 01101000 -> 104

-- Word8, Word16, Word32, Word64

exampleBytes :: [Word8]
exampleBytes = [104, 101, 108, 111] :: [Word8]

-- We can pack them (BS.pack)
