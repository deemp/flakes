module C_11_Streaming () where

import C_1_Handles (getDataDir)
import C_6_HTTP_types (MessageBody (MessageBody), Response (..))
import C_8_Responding (ok, sendResponse, status)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy qualified as LBS
import GHC.IO.Handle (Handle)
import GHC.IO.IOMode (IOMode (..))
import Network.Simple.TCP (HostPreference (..), serve)
import Text.Blaze.Html5 as Html ()
import System.FilePath ((</>))
import C_3_Bytes (binaryFileResource)

-- 11

hContentsResponse :: Handle -> IO Response
hContentsResponse h = do
  fileContent <- liftIO (LBS.hGetContents h)
  let body = Just (MessageBody fileContent)
  return (Response (status ok) [] body)

fileStrict :: IO b
fileStrict = do
  dir <- getDataDir
  serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
    (_, h) <-
      binaryFileResource (dir </> "stream.txt") ReadMode
    r <- liftIO (hContentsResponse h)
    liftIO (sendResponse s r)