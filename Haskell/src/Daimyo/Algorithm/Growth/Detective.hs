module Daimyo.Algorithm.Growth.Detective (
    detect,
    detect'N,
    detect'compare,
    tests
) where

import Daimyo.Algorithm.Growth
import Daimyo.NumberTheory.Factorial
import Data.List

{-
    experimental functions for finding the O(...) growth of a function
-}

detect'N f n = detect f [1..n]

detect f interval =
    let
        comparisons = map (\g -> detect'compare f interval g) growthFunctions
        sums = map (\(name,values) -> (name, sum values)) comparisons
        z = sum $ map f interval
    in
        minimumBy (\(name1,sum1) (name2,sum2) -> compare sum1 sum2) sums
--        (minimumBy (\(name1,sum1) (name2,sum2) -> compare sum1 sum2) sums, z, sums)

{-
    compare a function, up to n values, against g (a growth function structure)
-}
detect'compare f interval g =
    let
        results = gaps $ map f interval
        g'results = (_growthFn g) interval
    in
        (_name g, testGrowthFunctionResults results g'results)


tests =
    let
        interval = [10..100]
    in
        [
            detect (\x -> 3*x+100) interval,
            detect (\x -> 3*x) interval,
            detect (\x -> 3*(x*x)+100) interval,
            detect (\x -> 3*2**x) interval,
            detect (\x -> 3*(log x)+100) interval,
            detect (\x -> 3*(log (log x))+100) interval,
            detect (\x -> 10+sin x) interval,
            detect (\x -> 10+cos x) interval
        ]
