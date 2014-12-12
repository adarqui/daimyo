{-
    sources:

        Primes
            http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
            https://www.haskell.org/haskellwiki/Prime_numbers

        Primality Test
            http://en.wikipedia.org/wiki/Primality_test
            http://en.wikipedia.org/wiki/AKS_primality_test
-}

module Daimyo.Lib.Wave.NumberTheory.Prime (
    primes,
    primes'trial,
    primeFactors
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


{-
    isPrime

def isprime(n):
   """Returns True if n is prime"""
   if n == 2: return True
   if n == 3: return True
   if n % 2 == 0: return False
   if n % 3 == 0: return False

   i = 5
   w = 2
   while i * i <= n:
      if n % i == 0:
         return False

      i += w
      w = 6 - w

   return True
-}
