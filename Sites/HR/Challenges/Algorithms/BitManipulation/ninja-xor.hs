{-# OPTIONS -O2 #-}

import           Control.Monad
import           Data.Bits
import           Data.List

-- | solve
--
-- >>> solve [1,2,3,4]
-- 56
--
solve :: [Int] -> Int
solve nums = (foldl (\acc l -> acc + xorSum l) 0 $ subsequences nums) `mod` modp

-- | modp
--
modp :: Int
modp = ((10^9) + 7)

-- | xorSum
--
-- >>> xorSum [1,2,3,4]
-- 4
--
xorSum :: (Bits a, Num a) => [a] -> a
xorSum [] = 0
xorSum (x:xs) = x `xor` xorSum xs

-- | getPair
--
getPair :: IO (Int,[Int])
getPair = do
 n <- readLn :: IO Int
 nums <- getLine >>= \line -> return $ map (\n -> read n :: Int) $ words line
 return (n, nums)

-- | subseqs
--
-- >>> subseqs [1,2,3]
-- [[1],[1,2],[2],[1,2,3],[2,3],[3]]
--
subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  pairs <- mapM (const getPair) [1..test_cases]
  mapM_ (putStrLn . show . solve . snd) pairs
