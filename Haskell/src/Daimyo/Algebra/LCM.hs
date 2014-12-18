module Daimyo.Algebra.LCM (
    lcm
) where

import Daimyo.NumberTheory.GCD
import Prelude hiding (gcd, lcm)

lcm a b = (a * b) `div` (gcd a b)

t_lcm'1 = lcm 57 21
