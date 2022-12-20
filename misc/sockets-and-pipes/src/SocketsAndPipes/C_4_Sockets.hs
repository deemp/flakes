module C_4_Sockets (openAndConnect, resolve) where

import C_2_Chunks (repeatUntil)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (
  ReleaseKey,
  ResourceT,
  allocate,
  runResourceT,
 )
import Data.ByteString as BS (null, putStr)
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Simple.TCP (Socket)
import Network.Socket (Family (..))
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S

-- 4.1 Open up and connect

makeFriend :: S.SockAddr -> IO ()
makeFriend address = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s address
  S.sendAll s $
    T.encodeUtf8 $
      T.pack "Hello, will you be my friend?"
  repeatUntil (S.recv s 1024) BS.null BS.putStr

-- 4.2 Extra details

sec :: Int -> Int
sec t = t * 1000

sec1 :: Int
sec1 = sec 1

makeFriendSafely :: S.SockAddr -> IO ()
makeFriendSafely address = runResourceT @IO do
  (_, s) <-
    allocate
      (S.socket S.AF_INET S.Stream S.defaultProtocol)
      S.close
  liftIO do
    S.setSocketOption s S.UserTimeout sec1
    S.connect s address
    S.sendAll s $
      T.encodeUtf8 $
        T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1024) BS.null BS.putStr
    S.gracefulClose s sec1

-- 4.4 Address information

myHints :: S.AddrInfo
myHints = S.defaultHints{S.addrFamily = AF_INET6}

s1 :: IO ()
s1 = traverse_ print =<< S.getAddrInfo Nothing (Just "www.haskell.org") (Just "http")

s2 :: IO ()
s2 = traverse_ print =<< S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just "www.haskell.org") (Just "http")

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
  addrInfos <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just "www.haskell.org") (Just "http")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

makeFriendAddrInfo :: S.AddrInfo -> IO ()
makeFriendAddrInfo addressInfo = runResourceT @IO do
  (_, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout sec1
    S.connect s (S.addrAddress addressInfo)
    S.sendAll s $
      T.encodeUtf8 $
        T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1024) BS.null BS.putStr
    S.gracefulClose s sec1

mkFriend :: IO ()
mkFriend = makeFriendSafely (S.SockAddrInet 80 (S.tupleToHostAddress (147, 75, 54, 133)))

-- >>>mkFriend

-- 4.5 Exercises

--- Ex 10

openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addressInfo = do
  (r, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 1000
    S.connect s (S.addrAddress addressInfo)
  return (r, s)

--- Ex 11

findGopherWebsite :: IO S.AddrInfo
findGopherWebsite = do
  addrInfos <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just "gopher.floodgap.com") (Just "gopher")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

--- Ex 12

resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve sname hname = do
  addrInfos <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just hname) (Just sname)
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x