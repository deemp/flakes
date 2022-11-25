module C_4_4_Address_Information where

import C_4_2_Extra_details (makeFriendSafely)
import Network.Socket qualified as S

mkFriend :: IO ()
mkFriend = makeFriendSafely (S.SockAddrInet 80 (S.tupleToHostAddress (147, 75, 54, 133)))

-- >>>makeFriend