module Daimyo.NumberTheory.Fibonacci.Dynamic (
  boundsFib,
  compFib,
  fib
) where

import           Daimyo.Algorithm.BottomUp.Dynamic
import           Daimyo.Table.Array
import           GHC.Arr

-- | boundsFib
--
boundsFib :: (Ix a, Integral a) => a -> (a, a)
boundsFib n = (0, n)

-- | compFib
--
compFib :: (Ix a, Integral a) => Table a a -> a -> a
compFib t i
  | i <= 1    = i
  | otherwise = findTable (i-1) t + findTable (i-2) t

-- | fib
--
-- mostly taken from AAFA
--
-- >>> fib 10 :: Int
-- 55
--
-- >>> fib 1000 :: Int
-- 817770325994397771
--
-- >>> fib 1000 :: Integer
-- 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
--
fib :: (Ix a, Integral a) => a -> a
fib n = findTable n t
  where
    t = dynamic compFib (boundsFib n)
