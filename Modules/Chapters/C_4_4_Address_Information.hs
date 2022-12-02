module C_4_4_Address_Information where

import C_2_3_Exercises (repeatUntil)
import C_4_2_Extra_details (sec1)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Socket (Family (..))
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S

myHints :: S.AddrInfo
myHints = S.defaultHints{S.addrFamily = AF_INET6}

s1 = traverse_ print =<< S.getAddrInfo Nothing (Just "www.haskell.org") (Just "http")

s2 = traverse_ print =<< S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just "www.haskell.org") (Just "http")

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
  addrInfos <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just "www.haskell.org") (Just "http")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

makeFriendSafely :: S.AddrInfo -> IO ()
makeFriendSafely addressInfo = runResourceT @IO do
  (_, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout sec1
    S.connect s (S.addrAddress addressInfo)
    S.sendAll s $
      T.encodeUtf8 $
        T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1024) BS.null BS.putStr
    S.gracefulClose s sec1
