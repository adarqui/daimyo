module Daimyo.NumberTheory.Fibonacci.Variations (
  fib3,
  fibs3,
  fib4,
  fibs4,
  fib3',
  fib4',
  fibDp,
  fibsDp
) where

import           Daimyo.Algorithm.BottomUp.Dynamic
import           Daimyo.NumberTheory.Fibonacci.Dynamic
import           Daimyo.Table.Array
import           GHC.Arr

-- | fib3'
--
-- >>> fib3' 5 :: Int
-- 11
--
-- >>> fib3' 10 :: Int
-- 230
--
fib3' :: Integral a => a -> a
fib3' n
  | n <= 3    = n
  | otherwise = fib3' (n-1) + fib3' (n-2) + fib3' (n-3)

-- | fib3'
--
-- >>> fib4' 5 :: Int
-- 10
--
-- >>> fib4' 10 :: Int
-- 258
--
fib4' :: Integral a => a -> a
fib4' n
  | n <= 4    = n
  | otherwise = fib4' (n-1) + fib4' (n-2) + fib4' (n-3) + fib4' (n-4)

-- | compFib3
--
compFib3 :: (Ix a, Integral a) => Table a a -> a -> a
compFib3 t i
  | i <= 2    = i
  | otherwise = findTable (i-1) t + findTable (i-2) t + findTable (i-3) t

-- | compFib4
--
compFib4 :: (Ix a, Integral a) => Table a a -> a -> a
compFib4 t i
  | i <= 4    = i
  | otherwise = findTable (i-1) t + findTable (i-2) t + findTable (i-3) t + findTable (i-4) t

-- | fib3
--
-- >>> fib3 10 :: Int
-- 230
--
fib3 :: (Ix a, Integral a) => a -> a
fib3 = fibDp compFib3

-- | fibs3
--
-- >>> fibs3 20 :: [Int]
-- [0,1,2,3,6,11,20,37,68,125,230,423,778,1431,2632,4841,8904,16377,30122,55403,101902]
--
fibs3 :: (Ix a, Integral a) => a -> [a]
fibs3 = fibsDp compFib3

-- | fib4
--
-- >>> fib4 10 :: Int
-- 258
--
fib4 :: (Ix a, Integral a) => a -> a
fib4 = fibDp compFib4

-- | fibs4
--
-- >>> fibs4 20 :: [Int]
-- [0,1,2,3,4,10,19,36,69,134,258,497,958,1847,3560,6862,13227,25496,49145,94730,182598]
--
fibs4 :: (Ix a, Integral a) => a -> [a]
fibs4 = fibsDp compFib4

-- | fibDp
--
fibDp :: (Ix a, Integral a) => (Table a a -> a -> a) -> a -> a
fibDp comp n = findTable n t
  where
    t = dynamic comp (boundsFib n)

-- | fibsDp
--
fibsDp :: (Ix a, Integral a) => (Table a a -> a -> a) -> a -> [a]
fibsDp comp n = map snd $ toList t
  where
    t = dynamic comp (boundsFib n)
