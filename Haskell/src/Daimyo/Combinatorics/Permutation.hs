module Daimyo.Combinatorics.Permutation (
    permutations',
    permutations'',
    permutations'xX
) where

import Daimyo.Math.Factorial

import Data.List

{-
If X is an m-element set and n is a positive integer with m ≥ n, then the
number of X-strings of length n that are permutations is P(m, n).
-}

permutations' (m, n) = (fac m) `div` (fac $ m - n)

permutations'' m n = permutations' (m, n)

permutations'x2 m = m*(m-1)
permutations'x3 m = m*(m-1)*(m-2)

permutations'xX m 1 = m
permutations'xX m n = permutations'xX m (n-1) * (m-n+1)
