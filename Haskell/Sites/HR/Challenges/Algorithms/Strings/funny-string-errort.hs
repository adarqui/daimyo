{-
- HackerRank doesn't have transformers with ExceptT, thus, this code.
-}

{-
source: https://www.hackerrank.com/challenges/funny-string

Problem Statement

Suppose you have a string S which has length N and is indexed from 0 to N−1. String R is the reverse of the string S. The string S is funny if the condition |Si−Si−1|=|Ri−Ri−1| is true for every i from 1 to N−1.

(Note: Given a string str, stri denotes the ascii value of the ith character (0-indexed) of str. |x| denotes the absolute value of an integer x)

Input Format

First line of the input will contain an integer T. T testcases follow. Each of the next T lines contains one string S.

Constraints

1<=T<=10
2<=length of S<=10000
Output Format

For each string, print Funny or Not Funny in separate lines.

Sample Input

2
acxz
bcxz
Sample Output

Funny
Not Funny
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative       (Applicative)
import           Control.Monad             (forever)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.Error (Error, ErrorT, runErrorT,
                                            throwError)
import           Control.Monad.Trans.State (StateT, evalState, get, modify)
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as C
import           Data.Char                 (ord)
import           Prelude                   hiding (break, mapM, mapM_, sequence,
                                            void)
-------------------------------------------------------------------------------
-- Break - mostly from the https://hackage.haskell.org/package/break library --
-------------------------------------------------------------------------------

newtype Break r m a = Break { unBreak :: ErrorT r m a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    )

break :: Monad m => r -> Break r m a
break r = Break (throwError r)

breakIf :: (Monad m, Error r) => Bool -> r -> Break r m ()
breakIf cond r =
  if cond
    then Break (throwError r)
    else return ()

loop :: (Monad m, Error r) => Break r m () -> m r
loop m = do
    x <- runErrorT (unBreak (forever m))
    either return return x

----------------------------------------
-- Custom Control.Monad/Prelude stuff --
----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM act (x:xs) = do
  v  <- act x
  vs <- mapM act xs
  return $ v : vs

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ act xs = do
  void $ mapM act xs

void :: Monad m => m a -> m ()
void act = act >> return ()

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (act:acts) = do
  v  <- act
  vs <- sequence acts
  return $ v : vs

replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 _ = return []
replicateM n act = do
  v  <- act
  vs <- replicateM (n-1) act
  return $ v : vs

----------------
-- ByteString --
----------------

rindex :: ByteString -> Int -> Char
bs `rindex` i = bs `C.index` i'
  where
    i' = C.length bs - i

-------------
-- Amusing --
-------------

data Amusing = Funny | NotFunny

instance Show Amusing where
  show Funny = "Funny"
  show NotFunny = "Not Funny"

instance Error Amusing where

-- | amusingString evaluates the supplied bytestring to Funny or NotFunny
--
-- >>> amusingString "acxz"
-- Funny
--
-- >>> amusingString "bcxz"
-- Not Funny
amusingString :: ByteString -> Amusing
amusingString bs = evalState go 1
  where
  go = loop $ do
    i <- lift get
    void $ breakIf (i >= C.length bs) Funny
    let
      (c1, c2)   = (,) (ord $ bs `C.index` i) (ord $ bs `C.index` (i-1))
      (rc1, rc2) = (,) (ord $ bs `rindex` i)  (ord $ bs `rindex` (i+1))
    if (c1 - c2 == rc1 - rc2)
      then lift $ modify (+1)
      else break NotFunny

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  strings <- replicateM test_cases C.getLine
  mapM_ (putStrLn . show . amusingString) strings
