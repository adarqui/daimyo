module Daimyo.NumberTheory.Prime.Twin (
    twins,
    twins'even
) where

import Daimyo.NumberTheory.Prime

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
