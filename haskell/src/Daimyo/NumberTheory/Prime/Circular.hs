module Daimyo.NumberTheory.Prime.Circular (
  circularPrimes
) where

import           Daimyo.List.Pattern
import           Daimyo.Number
import           Daimyo.NumberTheory.Prime

-- | circularPrimes
--
-- https://en.wikipedia.org/wiki/Circular_prime
--
-- >>> take 20 circularPrimes
-- [2,3,5,7,11,13,17,31,37,71,73,79,97,113,131,197,199,311,337,373]
--
circularPrimes :: [Integer]
circularPrimes = filter (all (isPrime . digitsToNumber) . (rotations . digits)) primes
