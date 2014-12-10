{-
    sources:

        Primes
            http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
            https://www.haskell.org/haskellwiki/Prime_numbers
        GCD
            http://en.wikipedia.org/wiki/Greatest_common_divisor
-}

module Daimyo.Lib.Wave.NumberTheory (
    primes,
    gcd
) where

import Data.List

{-
    sieve
-}

primes = 2 : 3 : sieve 5 [5,7..]

sieve p (x:xs) =
    let
        xs' = [ x' | x' <- xs, x' `mod` p > 0]
    in
        p : sieve x xs'


{-
    trial division
-}

primes'trial = 2 : 3 : primes'trial' 5 [5,7..]
    where
        primes'trial' p rest =
            let
                (p':rest') = filter (\n -> n `mod` p /= 0) rest
            in
                p : primes'trial' p' rest'

{-
    GCD
-}

gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

gcf = gcd'
hcf = gcd'
gcm = gcd'
hcd = gcd'

gcd'' a b
    | a == b = a
    | a > b = gcd (a-b) b
    | b > a = gcd a (b-a)
