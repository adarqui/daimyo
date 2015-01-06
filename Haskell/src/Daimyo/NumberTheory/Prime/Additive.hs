module Daimyo.NumberTheory.Prime.Additive (
    isPrimeDigits,
    additive
) where

import Daimyo.Number
import Daimyo.NumberTheory.Prime

additive = filter isPrimeDigits primes

isPrimeDigits n = isPrime $ sum $ digits n
