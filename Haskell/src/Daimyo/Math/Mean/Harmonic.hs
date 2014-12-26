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

t_harmonic = harmonic [4,36,45,50,75]
