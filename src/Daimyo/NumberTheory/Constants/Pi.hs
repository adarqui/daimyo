module Daimyo.NumberTheory.Constants.Pi (
    pi'approx,
    pi'continuedFraction
) where

import Daimyo.NumberTheory.ContinuedFraction
import Daimyo.NumberTheory.Odd

-- http://www.futilitycloset.com/2010/05/12/pandigital-approximations/

pi'approx = 2**(5**0.4) - 0.6 - ((0.3^9) / 7.0) ** (0.8 ** 0.1)

pi'continuedFraction :: Integer -> Double
pi'continuedFraction precision =
    let
        a's = replicate (fromIntegral precision :: Int) 6
        numerators = [ fromIntegral (x*x) :: Double | x <- odds ]
    in
        continuedFractionNumerators (3, numerators, a's)
