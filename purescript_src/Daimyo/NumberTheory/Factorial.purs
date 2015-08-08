module Daimyo.NumberTheory.Factorial (
  factorial,
  factorialBig
) where

import Prelude ((-), (*), (==))
import Data.BigInt

-- | factorial
--
factorial :: Int -> Int
factorial = go 1
  where
  go acc 0 = acc
  go acc n = go (n*acc) (n-1)

-- | factorialBig
--
-- >>> factorialBig (fromInt 30)
-- fromString "265252859812191058636308480000000"
--
factorialBig :: BigInt -> BigInt
factorialBig = go (fromInt 1)
  where
  go acc n
    | n == fromInt 0    = acc
    | true              = go (n*acc) (n-(fromInt 1))
