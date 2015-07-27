module Daimyo.NumberTheory.Fibonacci.Variations (
  fib3,
  fib4,
  fib3',
  fib4'
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

-- | fib4
--
-- >>> fib4 10 :: Int
-- 258
--
fib4 :: (Ix a, Integral a) => a -> a
fib4 = fibDp compFib4

-- | fibDp
--
fibDp :: (Ix a, Integral a) => (Table a a -> a -> a) -> a -> a
fibDp comp n = findTable n t
  where
    t = dynamic comp (boundsFib n)
