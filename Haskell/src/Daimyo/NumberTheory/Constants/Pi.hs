module Daimyo.NumberTheory.Constants.Pi (
    pi'approx
) where

-- http://www.futilitycloset.com/2010/05/12/pandigital-approximations/

pi'approx = 2**(5**0.4) - 0.6 - ((0.3^9) / 7.0) ** (0.8 ** 0.1)
