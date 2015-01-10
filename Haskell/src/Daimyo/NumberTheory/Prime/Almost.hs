module Daimyo.NumberTheory.Prime.Almost (
    two'almost'primes,
    three'almost'primes,
    n'almost'primes,
    semiprimes
) where

import Daimyo.NumberTheory.Prime

two'almost'primes = n'almost'primes 2

three'almost'primes = n'almost'primes 3

semiprimes = two'almost'primes

n'almost'primes n = filter (\p -> length (primeFactors' p) == n) [2..]
