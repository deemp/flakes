module C_5_HTTP (crlf, helloRequestString, helloResponseString) where

import ASCII qualified as A
import ASCII.Char qualified as A
import C_4_Sockets (openAndConnect, resolve)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString qualified as BS
import Network.Simple.TCP (HostPreference (..))
import Network.Simple.TCP qualified as Net
import Relude (putBSLn)

-- 5.3 ASCII strings

line :: BS.ByteString -> BS.ByteString
line x = x <> A.lift crlf

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

text :: [BS.ByteString] -> BS.ByteString
text = foldMap line

helloRequestString :: BS.ByteString
helloRequestString =
  text
    [ [A.string|GET /hello.txt HTTP/1.1|]
    , [A.string|Host: www.example.com|]
    , [A.string|Accept-Language: en, mi|]
    , [A.string||]
    ]

-- 5.4 HTTP responses

helloResponseString :: BS.ByteString
helloResponseString =
  text
    [ [A.string|HTTP/1.1 200 OK|]
    , [A.string|Content-Type: text/plain; charset=us-ascii|]
    , [A.string|Content-Length: 6|]
    , [A.string||]
    ]
    <> [A.string|Hello|]

-- 5.5 Serving others

p :: MonadIO m => HostPreference -> Net.ServiceName -> ((Net.Socket, Net.SockAddr) -> IO ()) -> m a
p = Net.serve

ourFirstServer :: IO a
ourFirstServer = Net.serve @IO HostAny "8000" \(s, a) -> do
  putStrLn ("New connection from " <> show a)
  Net.send s helloResponseString

-- 5.6 Exercises

--- Ex 13

repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m x) -> m ()
repeatUntilNothing getChunkMaybe f = continue
 where
  continue = do getChunkMaybe >>= maybe (return ()) (\x -> f x >> continue)

--- Ex 14

requestText :: BS.ByteString
requestText =
  text
    [ [A.string|GET / HTTP/1.1|]
    , [A.string|Host: haskell.org|]
    , [A.string|Connection: close|]
    ]

requestHaskellOrg :: IO ()
requestHaskellOrg = runResourceT @IO do
  addrInfo <- liftIO $ resolve "https" "haskell.org"
  (_, s) <- openAndConnect addrInfo
  Net.send s requestText
  repeatUntilNothing (Net.recv s 1024) (liftIO . putBSLn)
