module Daimyo.NumberTheory.Prime.SophieGermain (
    sophieGermainPrimes,
    isSophieGermainPrime
) where

import Daimyo.NumberTheory.Prime

sophieGermainPrimes = filter isSophieGermainPrime primes

isSophieGermainPrime p = isPrime p && isPrime (2*p+1)
