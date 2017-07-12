module Daimyo.NumberTheory.Factorial.CPS (
  factorialCPS,
  Factorial (..)
) where

-- | factorialCPS
--
-- >>> factorialCPS 5 id :: Int
-- 120
--
factorialCPS :: (Eq a, Num a) => a -> (a -> r) -> r
factorialCPS 0 k = k 1
factorialCPS n k = factorialCPS (n-1) $ \ret -> k (n*ret)

newtype Factorial a r = Factorial { runFactorial :: (a -> r) }
