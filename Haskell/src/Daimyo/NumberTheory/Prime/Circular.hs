module Daimyo.NumberTheory.Prime.Circular (
  circular
) where

import           Daimyo.List.Pattern
import           Daimyo.Number
import           Daimyo.NumberTheory.Prime

-- | circular
--
-- https://en.wikipedia.org/wiki/Circular_prime
--
-- >>> take 20 circular
-- [2,3,5,7,11,13,17,31,37,71,73,79,97,113,131,197,199,311,337,373]
--
circular :: [Integer]
circular = filter (all (isPrime . digitsToNumber) . (rotations . digits)) primes
