module C_5_3_ASCII_strings where

import C_5_4_HTTP_responses (helloResponseString)
import Control.Monad.IO.Class (MonadIO)
import Network.Simple.TCP (HostPreference (..))
import Network.Simple.TCP qualified as Net

p :: MonadIO m => HostPreference -> Net.ServiceName -> ((Net.Socket, Net.SockAddr) -> IO ()) -> m a
p = Net.serve

ourFirstServer :: IO a
ourFirstServer = Net.serve @IO HostAny "8000" \(s, a) -> do
  putStrLn ("New connection from " <> show a)
  Net.send s helloResponseString