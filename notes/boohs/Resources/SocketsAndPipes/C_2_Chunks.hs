module C_2_Chunks (repeatUntil) where

import C_1_Handles (fileResource, getDataDir, greetingTxt)
import Control.Monad ()
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Handle.FD (stdout)
import GHC.IO.IOMode (IOMode (WriteMode))
import Relude (IOMode (ReadMode))
import System.FilePath ((</>))
import System.IO qualified as IO

-- 2.1 Packed characters

helloText :: IO ()
helloText = T.hPutStrLn stdout (T.pack "hello, world!")

helloTextFile :: IO ()
helloTextFile = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> greetingTxt) WriteMode
  liftIO do
    T.hPutStrLn h (T.pack "hello")
    T.hPutStrLn h (T.pack "world")

{-
>>>helloTextFile
-}

-- Text is strict
-- this crashes
p :: T.Text
p = T.take 10 (T.pack (cycle "abc"))

-- 2.2 Reading from a file, one chunk at a time

printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> greetingTxt) ReadMode
  liftIO $
    repeatUntilIO (T.hGetChunk h) T.null $
      \chunk -> T.putStr (T.toUpper chunk)

printCapitalizedText :: IO.Handle -> IO ()
printCapitalizedText h = continue
 where
  continue = do
    chunk <- T.hGetChunk h
    unless
      (T.null chunk)
      ( do
          T.putStr (T.toUpper chunk)
          continue
      )

repeatUntilIO ::
  IO chunk ->
  (chunk -> Bool) ->
  (chunk -> IO x) ->
  IO ()
repeatUntilIO getChunk isEnd f = continue
 where
  continue = do
    chunk <- getChunk
    unless
      (isEnd chunk)
      ( do
          _ <- f chunk
          continue
      )

--- Ex 4

digitsOnly :: Text -> Text
digitsOnly = T.filter isDigit

testDigitsOnly :: Text
testDigitsOnly = digitsOnly (T.pack "ab c123 def4")

{-
>>>testDigitsOnly
"1234"
-}

capitalizeLast :: Text -> Text
capitalizeLast t = T.init t <> T.toUpper (T.takeEnd 1 t)

{-
>>>capitalizeLast ","

>>>capitalizeLast "a"
-}

unParen :: Text -> Maybe Text
unParen t
  | T.length t < 2 = Nothing
  | pref == '(' && suff == ')' = Just body
  | otherwise = Nothing
 where
  pref = T.head t
  suff = T.last t
  body = T.init (T.tail t)

{-
>>>unParen ""
Nothing

>>>unParen "(a)"
Just "a"
-}

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

{-
>>>characterCount "greeting.txt"
12
-}

--- Ex 6

when :: Monad m => Bool -> m () -> m ()
when cond action = if cond then action else return ()

unless :: Monad m => Bool -> m () -> m ()
unless cond = when (not cond)

--- Ex 7

repeatUntil :: Monad m => m chunk -> (chunk -> Bool) -> (chunk -> m x) -> m ()
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
