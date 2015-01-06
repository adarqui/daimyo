module Daimyo.NumberTheory.Prime.Chen (
    chen
) where

import Daimyo.NumberTheory.Prime
import Daimyo.NumberTheory.Prime.Semiprime

chen = [ p | p <- primes, let p2 = p+2 in isPrime p2  || isSemiPrime p2 ]
