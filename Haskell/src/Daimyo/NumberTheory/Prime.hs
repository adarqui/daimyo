{-
    sources:

        Primes
            http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
            https://www.haskell.org/haskellwiki/Prime_numbers

        Primality Test
            http://en.wikipedia.org/wiki/Primality_test
            http://en.wikipedia.org/wiki/AKS_primality_test
-}

module Daimyo.NumberTheory.Prime (
    primes,
    primes'trial,
    primeFactors,
    isPrime,
    primes',
    isFactor,
    nth'sieve,
    primeFactors'
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
    Stolen
    https://www.haskell.org/haskellwiki/99_questions/Solutions/35
-}

primeFactors :: Integer -> [Integer]
primeFactors a = let (f, f1) = factorPairOf a
                     f' = if prime f then [f] else primeFactors f
                     f1' = if prime f1 then [f1] else primeFactors f1
                 in f' ++ f1'
 where
 factorPairOf a = let f = head $ factors a
                  in (f, a `div` f)
 factors a    = filter (isFactor a) [2..a-1]
 isFactor a b = a `mod` b == 0
 prime a      = null $ factors a

isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes

primes' = 2 : filter isPrime [3,5..]

primeFactors' n | n > 1 = go n primes'
 where
     go n ps@(p:t)
        | p*p > n    = [n]
        | r == 0     =  p : go q ps
        | otherwise  =      go n t
                where
                  (q,r) = quotRem n p

isFactor fac n = n `rem` fac == 0

sieve' :: [Integer]
sieve' = sieve' [2..]
  where
    sieve' (p:xs) = p : sieve' [x|x <- xs, x `mod` p > 0]

nth'sieve n = sieve' !! n
