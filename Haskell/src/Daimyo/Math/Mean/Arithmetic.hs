module Daimyo.Math.Mean.Arithmetic (
    arithmetic'mean,
    arithmetic
) where

import Data.List

arithmetic'mean l =
    let
        n = length l
        sum' = foldl' (+) 0 l
    in
        sum' / fromIntegral n

arithmetic = arithmetic'mean

t_arithmetic'1 = arithmetic'mean [4,36,45,50,75]
t_arithmetic'2 = arithmetic'mean [3,3,3]
t_arithmetic'3 = arithmetic'mean [1,2,3]
