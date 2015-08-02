module Pure.NumberTheory.Factorial (
  factorial
) where

import Prelude ((-), (*))

-- | factorial
--
factorial :: Int -> Int
factorial = go 1
  where
  go acc 0 = acc
  go acc n = go (n*acc) (n-1)
