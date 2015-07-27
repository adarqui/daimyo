module Daimyo.NumberTheory.Prime.Circular (
    circular
) where

import Daimyo.Number
import Daimyo.NumberTheory.Prime
import Daimyo.List.Pattern

import Data.List

circular = filter (\p -> all (isPrime . digitsToNumber) (rotations $ digits p)) primes
