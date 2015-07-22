module Daimyo.Algebra.Exponent (
  negPow
) where

-- | negPow: for my bernoulli functions apparently
--
-- >>> negPow 2 (-2) :: Double
-- 0.25
--
negPow :: (RealFrac a, Floating a) => a -> a -> a
negPow x n
  | x < 0     = firstCase
  | otherwise = x ** n
  where
    absx = abs x
    firstCase =
      if (odd $ floor n)
        then (-1)*(absx**n)
        else absx**n
