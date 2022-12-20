module C_3_Bytes (binaryFileResource) where

import Data.Word (Word8)

import C_1_Handles (getDataDir, greetingTxt)
import C_2_Chunks (repeatUntil)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Data.ByteString as BS (ByteString, hGetSome, hPut, hPutStr, map, null, pack)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle.FD (stdout)
import Relude (Handle, IOMode (..), UnicodeException)
import System.FilePath ((</>))
import System.IO qualified as IO

-- 3.1 Packed octets

exampleBytes :: [Word8]
exampleBytes = [104, 101, 108, 111] :: [Word8]

-- 3.2 Copying a file

greeting2Txt :: FilePath
greeting2Txt = "greeting2.txt"

copyGreetingFile :: IO ()
copyGreetingFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h1) <- binaryFileResource (dir </> greetingTxt) ReadMode
  (_, h2) <- binaryFileResource (dir </> greeting2Txt) WriteMode
  liftIO $ repeatUntil (BS.hGetSome h1 1024) BS.null (BS.hPutStr h2)

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

-- 3.5 Avoiding system defaults

helloHandle :: IO ()
helloHandle = IO.hPutStrLn IO.stdout "Hello, world!"

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

-- 3.6 Exercises

--- Ex 8

greet :: BS.ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
  Left _ -> putStrLn "Invalid byte string"
  Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)

p1 :: Either UnicodeException Text
p1 = T.decodeUtf8' (fromString "♫")

-- >>>p1
-- Right "k"

--- Ex 9

asciiUpper :: BS.ByteString -> BS.ByteString
asciiUpper = BS.map (\x -> if 97 <= x && x <= 122 then 65 + x - 97 else x)

p2 :: ByteString
p2 = asciiUpper (fromString "Hello!")

-- >>> p2
-- "HELLO!"
