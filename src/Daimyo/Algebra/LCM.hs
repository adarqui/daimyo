module Daimyo.Algebra.LCM (
    lcm
) where

import           Daimyo.NumberTheory.GCD
import           Prelude                 hiding (gcd, lcm)

-- | lcm
--
-- >>> lcm 10 15 :: Integer
-- 30
--
-- >>> lcm 57 21 :: Integer
-- 399
--
lcm :: Integral a => a -> a -> a
lcm a b = a * b `div` gcd a b
