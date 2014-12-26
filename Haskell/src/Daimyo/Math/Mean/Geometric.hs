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

t_geometric = geometric [4,36,45,50,75]
