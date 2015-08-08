module Daimyo.NumberTheory.Prime.Bertrand (
    bertrand
) where

import Daimyo.NumberTheory.Prime

bertrand p
    | isPrime p == False = error "can only pass primes"
    | p <= 3 = error "can only pass primes greater than 3"
    | otherwise =
        let
            ps = takeWhile (\p' -> p' < (2*p-2)) $ sieve'init p
        in
            if (not $ null ps)
                then Just ps
                else Nothing
