module Daimyo.Algebra.Divisibility.Common (
    commonDivisor
) where

-- | commonDivisor: is d a common divisor of a and b?d
--
-- >>> commonDivisor (2 :: Int) 10 12
-- True
--
-- >>> commonDivisor (5 :: Int) 10 12
-- False
--
commonDivisor :: Integral a => a -> a -> a -> Bool
commonDivisor d a b = a `mod` d == 0 && b `mod` d == 0
