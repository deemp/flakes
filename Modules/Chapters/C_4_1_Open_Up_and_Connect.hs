module C_4_1_Open_Up_and_Connect where

import C_2_3_Exercises (repeatUntil)
import Data.ByteString as BS (null, putStr)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.Socket qualified as S
import Network.Socket.ByteString qualified as S

makeFriend :: S.SockAddr -> IO ()
makeFriend address = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s address
  S.sendAll s $
    T.encodeUtf8 $
      T.pack "Hello, will you be my friend?"
  repeatUntil (S.recv s 1024) BS.null BS.putStr