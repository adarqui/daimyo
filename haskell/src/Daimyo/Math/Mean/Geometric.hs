module Daimyo.Math.Mean.Geometric (
    geometric,
    geometric'mean
) where

import Daimyo.Algebra.NthRoot

geometric'mean l =
    let
        prod' = product l
        n = fromIntegral $ length l
    in
        nthRoot n prod'

geometric = geometric'mean

t_geometric'1 = geometric [4,36,45,50,75]
t_geometric'2 = geometric [3,3,3]
t_geometric'3 = geometric [1,2,3]
