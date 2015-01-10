module Daimyo.NumberTheory.Aliquot (
    aliquot'parts,
    amicable'pair,
    aliquot'sequence,
    aliquot'perfect
) where

import Daimyo.Algebra.Divisibility

aliquot'parts = proper'divisors

amicable'pair = undefined

aliquot'sequence n
    | n == 1 = [1]
    | otherwise =
        let
            ap = sum $ aliquot'parts n
        in
            n : aliquot'sequence ap

aliquot'perfect n =
    let
        as = aliquot'sequence n
        len = length $ take 2 $ takeWhile (==n) as
    in
        case len of
            2 -> True
            otherwise -> False


t_aliquot'parts'28 = aliquot'parts 28
t_aliquot'sequence'24 = aliquot'sequence 24
t_aliquot'sequence'28 = aliquot'sequence 28
