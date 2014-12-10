module Daimyo.Lib.Wave.Combinatorics.Combination (
    choose,
    choose'
) where

import Daimyo.Lib.Wave.Combinatorics.Permutation
import Daimyo.Lib.Math.Factorial

n `choose` k = (permutations' (n, k)) `div` (fac k)

n `choose'` k = (fac n) `div` ((fac k) * fac (n - k))
