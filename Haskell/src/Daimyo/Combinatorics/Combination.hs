module Daimyo.Combinatorics.Combination (
    choose,
    choose',
    chooseFrom,
    combos,
    combinations,
    chooseRepetition
) where

import Daimyo.Combinatorics.Permutation
import Daimyo.Math.Factorial

n `choose` k = (permutations' (n, k)) `div` (fac k)

n `choose'` k = (fac n) `div` ((fac k) * fac (n - k))

k `chooseFrom` nl = combos nl k

combos :: [a] -> Int -> [[a]]
combos members 1 = map (:[]) members
combos members n = concatMap (\front -> map (front ++) (combos members 1)) $ combos members (n - 1)

combinations = combos

t_chooseFrom = 3 `chooseFrom` [0,1]


n `chooseRepetition` k = (fac (n + k - 1)) `div` ((fac k)*(fac (n-1)))
