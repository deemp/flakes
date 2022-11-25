module C_1_4 where

import Control.Monad.IO.Class (MonadIO (..))
import System.IO qualified as IO

helloWorld :: MonadIO m => m ()
helloWorld = liftIO (IO.putStrLn "hello, world")
