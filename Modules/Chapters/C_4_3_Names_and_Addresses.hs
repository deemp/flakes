module C_4_3_Names_and_Addresses where

import C_4_2_Extra_details (makeFriendSafely)
import Network.Socket qualified as S

mkFriend :: IO ()
mkFriend = makeFriendSafely (S.SockAddrInet 80 (S.tupleToHostAddress (147, 75, 54, 133)))

-- >>>makeFriend