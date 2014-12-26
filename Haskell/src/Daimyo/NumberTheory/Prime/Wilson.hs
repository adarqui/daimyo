module Daimyo.NumberTheory.Prime.Wilson (
    wilson
) where

import Daimyo.Math.Factorial (fac'product)
import Daimyo.Algebra.Congruence (isCongruent)

wilson p = isCongruent (fac'product (p-1)) (-1) p

t_wilson'17 = wilson 17
t_wilson'16 = wilson 16
