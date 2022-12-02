module C_5_6_Exercises where

import ASCII qualified as A
import C_4_5_Exercises (openAndConnect, resolve)
import C_5_3_ASCII_strings (mapConcat)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString qualified as BS
import Network.Simple.TCP qualified as Net
import Relude (putBSLn)

repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m x) -> m ()
repeatUntilNothing getChunkMaybe f = continue
 where
  continue = do getChunkMaybe >>= maybe (return ()) (\x -> f x >> continue)

requestText :: BS.ByteString
requestText =
  mapConcat
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
