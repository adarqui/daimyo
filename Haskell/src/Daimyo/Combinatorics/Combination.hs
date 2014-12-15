module Daimyo.Combinatorics.Combination (
    choose,
    choose'
) where

import Daimyo.Combinatorics.Permutation
import Daimyo.Math.Factorial

n `choose` k = (permutations' (n, k)) `div` (fac k)

n `choose'` k = (fac n) `div` ((fac k) * fac (n - k))
