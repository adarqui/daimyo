module Daimyo.NumberTheory.Prime.Twin (
    twins,
    twins'even,
    twins'even'divisible'by'6
) where

import Daimyo.NumberTheory.Prime

import Data.List


{-
    twins: returns pairs of twin primes [(3,5),(5,7)...]
-}

twins = filter (\(x,y) -> x+2 == y) primes'pairs


{-
    twins'even: returns the 'even' that lies between a pair of twin primes
-}

twins'even = map (\(x,_) -> x+1) twins


{-
    TODO
    Brun Constant
    Twin Prime Theorem
-}


-- everything divisible by 6? except the first one, 4, hence the tail
twins'even'divisible'by'6 n = all (\k -> k `mod` 6 == 0) $ tail $ take n twins'even
