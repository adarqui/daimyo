{-# LANGUAGE NPlusKPatterns #-}

{-
    sources:

        http://en.wikipedia.org/wiki/Binomial_theorem
-}

module Daimyo.Lib.Wave.AbstractAlgebra.Polynomial (
    fac,
    binomial,
    pascal'data,
    t_pascal'data
) where

import Daimyo.Lib.Wave.Combinatorics.Combination

fac 0 = 1
fac (n+1) = (n+1) * fac n

{-
    (x + y)^n = sum from k=0 to n of(n `choose` k)x^(n-k)y^k
    (x + y)^n = sum from k=0 to n of(n `choose` k)x^ky^(n-k)
-}

binomial x y n =
    let
        formula k = (n `choose` k) * ((x^(n-k))*(y^k))
    in
        map formula [0..n]

pascal'data x y n = map (binomial x y) [1..n]

--pretty'pascal'data =

t_pascal'data = pascal'data 1 1 10
