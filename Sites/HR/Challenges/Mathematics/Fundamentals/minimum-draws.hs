{-

https://www.hackerrank.com/challenges/minimum-draws

Sample Input

2
1
2

-}

module Main where

import           Control.Applicative
import           Prelude             hiding (mapM, replicateM)

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
  v  <- f x
  vs <- mapM f xs
  return $ v : vs

replicateM :: (Monad m) => m a -> Int -> m [a]
replicateM _ 0 = return []
replicateM act n = do
  v  <- act
  vs <- replicateM act (n-1)
  return $ v : vs

void :: (Monad m) => m a -> m ()
void act = do
  _ <- act
  return ()

worstCase :: Int -> Int
worstCase 0 = 0
worstCase n = (1+n)

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  sock_pairs <- replicateM (readLn :: IO Int) test_cases
  void $ mapM (putStrLn . show . worstCase) sock_pairs
