module Daimyo.Algebra.Permutation.Cycle (
    kcycle,
    cyclek,
    cycle2,
    cycle3
) where

import Data.List

kcycle k s =
    let
        a = take k s
        b = drop k s
    in
        map (++ b) $ permutations a

cyclek = kcycle

cycle2 = kcycle 2
cycle3 = kcycle 3
