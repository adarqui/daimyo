module Daimyo.NumberTheory.Prime.Additive (
  isPrimeDigits,
  additivePrimes
) where

import           Daimyo.Number
import           Daimyo.NumberTheory.Prime

-- | additivePrimes
--
-- >>> take 20 additivePrimes
-- [2,3,5,7,11,23,29,41,43,47,61,67,83,89,101,113,131,137,139,151]
--
additivePrimes :: [Integer]
additivePrimes = filter isPrimeDigits primes

-- | isPrimeDigits
--
-- >>> isPrimeDigits 131
-- True
--
-- >>> isPrimeDigits 132
-- False
--
isPrimeDigits :: Integer -> Bool
isPrimeDigits = isPrime . sum . digits
