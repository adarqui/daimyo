module Daimyo.Math.Primes (
 isPrime,
 primes,
 primeFactors,
 isFactor,
 sieve,
 nth'sieve
) where

isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes

primes = 2 : filter isPrime [3,5..]

primeFactors n | n > 1 = go n primes
 where
     go n ps@(p:t)
        | p*p > n    = [n]
        | r == 0     =  p : go q ps
        | otherwise  =      go n t
                where
                  (q,r) = quotRem n p

isFactor fac n = n `rem` fac == 0

sieve :: [Integer]
sieve = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

nth'sieve n = sieve !! n
