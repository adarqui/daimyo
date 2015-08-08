module Daimyo.NumberTheory.Prime.Semiprime (
    semiPrimes,
    squareFree,
    squares,
    isSemiPrime
) where

import Daimyo.List.Safe
import Daimyo.NumberTheory.Prime

semiPrimes = [ n | n <- coprimes, let l = length (primeFactors' n) in l == 2 ]
squareFree = [ n | n <- coprimes, let all@(x:xs) = primeFactors' n in length all == 2 && x /= (head xs) ]
squares = [ p^2 | p <- primes ]

{-
    futile:
    squares = [ n | n <- coprimes, let all@(x:xs) = primeFactors' n in length all == 2 && x == (head xs) ]
-}

isSemiPrime p =
    let
        pfacs = primeFactors p
    in
        length pfacs == 2
