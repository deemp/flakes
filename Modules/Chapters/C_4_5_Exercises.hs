module C_4_5_Exercises where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate)
import Network.Simple.TCP (Socket)
import Network.Socket qualified as S

findGopherWebsite :: IO S.AddrInfo
findGopherWebsite = do
  addrInfos <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just "gopher.floodgap.com") (Just "gopher")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve sname hname = do
  addrInfos <- S.getAddrInfo (Just S.defaultHints{S.addrSocketType = S.Stream}) (Just hname) (Just sname)
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addressInfo = do
  (r, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 1000
    S.connect s (S.addrAddress addressInfo)
  return (r, s)