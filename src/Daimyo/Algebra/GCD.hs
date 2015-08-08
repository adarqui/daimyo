module Daimyo.Algebra.GCD (
  module Daimyo.NumberTheory.GCD,
  euclid
) where

import           Daimyo.NumberTheory.GCD

-- | euclid
--
-- >>> euclid 27 18
-- 9
--
euclid :: Integer -> Integer -> Integer
euclid = gcd
