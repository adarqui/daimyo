module Daimyo.NumberTheory.Factor (
    factor'pairs,
    factor'pairs'
) where

import Daimyo.List.Misc
import Daimyo.NumberTheory.Prime
import Data.List

factor'pairs n =
    case (isPrime n || n < 2) of
        True -> []
        False ->
            let
                pfs = primeFactors n
            in
                ([n], [1]) : rests pfs

{-
    multiplied
-}

factor'pairs' n = nub $ map (\(x,y) -> (product x, product y)) $ factor'pairs n
