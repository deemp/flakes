module C_3_2_Copying_a_file where

import C_0_Setup (getDataDir)
import C_1_2_Writing_to_a_file (greetingTxt)
import C_2_3_Exercises (repeatUntil)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Data.ByteString as BS (hGetSome, hPutStr, null)
import Relude (Handle, IOMode (..))
import System.FilePath ((</>))
import System.IO qualified as IO

greeting2Txt :: FilePath
greeting2Txt = "greeting2.txt"

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

copyGreetingFile :: IO ()
copyGreetingFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h1) <- binaryFileResource (dir </> greetingTxt) ReadMode
  (_, h2) <- binaryFileResource (dir </> greeting2Txt) WriteMode
  liftIO $ repeatUntil (BS.hGetSome h1 1024) BS.null (BS.hPutStr h2)