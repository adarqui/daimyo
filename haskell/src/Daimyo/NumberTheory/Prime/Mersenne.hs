module Daimyo.NumberTheory.Prime.Mersenne (
    mersennes
) where

import Daimyo.NumberTheory.Prime

mersennes = [ mersenne q | q <- primes, isPrime $ mersenne q ]

mersenne q = (2^q) - 1
