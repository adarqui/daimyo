module Daimyo.NumberTheory.Constants.E (
    e'precision,
    e'sequence,
    e'approx,
    e'continuedFraction,
    e'continuedFraction'abbreviatedForm,
    e'continuedFraction'abbreviatedForm'evaluated,
    e
) where

{-
    source: http://mathworld.wolfram.com/topics/e.html
-}

import Daimyo.Math.Factorial
import Daimyo.NumberTheory.ContinuedFraction
import Daimyo.NumberTheory.ContinuedFraction.Constant

e = 2.718281828459045235

e'precision precision = sum $ e'sequence precision

e'sequence k = map (\kfac -> 1/kfac) $ fac'list k


-- http://www.futilitycloset.com/2010/05/12/pandigital-approximations/ 

e'approx = (1 + 9^(-4^(6*6)))^(3^(2^(85)))


-- continued fraction style, minimum precision
e'continuedFraction = _e


-- e continued fraction, abbreviatedForm
e'continuedFraction'abbreviatedForm precision =
    let
        cycle = concat $ [1,2] : [ [1,1,x+2] | x <- [2,4..precision] ]
    in
        (2, cycle)

e'continuedFraction'abbreviatedForm'evaluated precision = continuedFraction $ e'continuedFraction'abbreviatedForm precision
