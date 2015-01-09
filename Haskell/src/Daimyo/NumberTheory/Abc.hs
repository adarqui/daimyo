module Daimyo.NumberTheory.Abc (
    r,
    radical
) where

import Daimyo.NumberTheory.Prime
import Data.List

{-
    r(n) = sum of distinct prime factors

    radical of n
-}

r n = product $ nub $ primeFactors n

radical = r
