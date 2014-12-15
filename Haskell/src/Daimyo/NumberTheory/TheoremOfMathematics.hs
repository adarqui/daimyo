module Daimyo.NumberTheory.TheoremOfMathematics (
    tom
) where

import Daimyo.NumberTheory.Prime

tom = map (\i -> (i, primeFactors' i)) [2..]
