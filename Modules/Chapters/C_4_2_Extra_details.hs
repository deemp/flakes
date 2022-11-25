module C_4_2_Extra_details where

import C_2_3_Exercises (repeatUntil)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Data.ByteString as BS (null, putStr)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S

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

-- >>>makeFriendSafely (S.SockAddrInet 80 (S.tupleToHostAddress (147, 75, 54, 133)))