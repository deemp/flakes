module C_2_3(repeatUntil) where

import C_0 (getDataDir)
import C_1_5 (fileResource, writeGreetingSafe)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Relude (IOMode (ReadMode))
import System.FilePath ((</>))

-- Ex 4

digitsOnly :: Text -> Text
digitsOnly = T.filter isDigit

-- >>>digitsOnly (T.pack "ab c123 def4")
-- "1234"

capitalizeLast :: Text -> Text
capitalizeLast t = T.init t <> T.toUpper (T.takeEnd 1 t)

-- >>>capitalizeLast ","
-- ","
-- >>>capitalizeLast "a"
-- "A"

unParen :: Text -> Maybe Text
unParen t
  | T.length t < 2 = Nothing
  | p == '(' && s == ')' = Just b
  | otherwise = Nothing
 where
  p = T.head t
  s = T.last b
  b = T.init (T.tail t)

-- >>>unParen ""
-- Nothing
-- >>>unParen "(a)"
-- Just "a"

-- Ex 5

characterCount :: FilePath -> IO Int
characterCount fp = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> fp) ReadMode
  liftIO $ continue (T.hGetChunk h) T.null 0
 where
  continue :: IO Text -> (Text -> Bool) -> Int -> IO Int
  continue getChunk isEnd n = do
    chunk <- getChunk
    if isEnd chunk
      then return n
      else continue getChunk isEnd (n + T.length chunk)

-- >>>writeGreetingSafe >> characterCount "greeting.txt"
-- 12

-- Ex 6
when :: Monad m => Bool -> m () -> m ()
when cond action = if cond then action else return ()

unless :: Monad m => Bool -> m () -> m ()
unless cond = when (not cond)

-- Ex 7

repeatUntil ::
  Monad m =>
  m chunk ->
  (chunk -> Bool) ->
  (chunk -> m x) ->
  m ()
repeatUntil getChunk isEnd f = continue
 where
  continue = do
    chunk <- getChunk
    unless
      (isEnd chunk)
      ( do
          _ <- f chunk
          continue
      )