module Daimyo.Math.Mean.Harmonic (
    harmonic,
    harmonic'mean
) where

import Daimyo.Algebra.NthRoot
import Data.List

harmonic'mean l =
    let
        sum' = foldl' (\acc y -> acc + (1/y)) 0 l
        n = fromIntegral $ length l
    in
        n * (1/sum')

harmonic = harmonic'mean

t_harmonic'1 = harmonic [4,36,45,50,75]
t_harmonic'2 = harmonic [3,3,3]
t_harmonic'3 = harmonic [1,2,3]
