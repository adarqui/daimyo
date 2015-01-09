module Daimyo.NumberTheory.Abundant (
    abundants,
    abundant,
    deficients
) where

import Daimyo.NumberTheory.Prime
import Data.List

abundants = [ n | n <- [2..], let ab = abundant n in sum ab > n ]
abundant n = 1 : (filter (/= n) $ primeMultiples'products n)

deficients = [ 2*n+1 | n <- [0..] ]

t_abundant'16 = abundant 16
