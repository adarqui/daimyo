module Daimyo.NumberTheory.Constants.E (
    e'precision,
    e'sequence,
    e'approx,
    e
) where

{-
    source: http://mathworld.wolfram.com/topics/e.html
-}

import Daimyo.Math.Factorial

e = 2.718281828459045235

e'precision precision = sum $ e'sequence precision

e'sequence k = map (\kfac -> 1/kfac) $ fac'list k

-- http://www.futilitycloset.com/2010/05/12/pandigital-approximations/ 

e'approx = (1 + 9^(-4^(6*6)))^(3^(2^(85)))
