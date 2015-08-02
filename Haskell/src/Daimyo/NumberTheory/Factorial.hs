module Daimyo.NumberTheory.Factorial (
  factorial,
  factorialProduct,
  rationalFactorial
) where

-- | factorial
--
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

-- factorialProduct
--
factorialProduct :: (Enum a, Num a) => a -> a
factorialProduct n = product [1..n]

rationalFactorial n
  | n < 1 = 1
  | otherwise = n * rationalFactorial (n-1)
