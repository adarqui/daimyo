module Daimyo.NumberTheory.Prime.Cousin (
    cousins
) where

import Daimyo.NumberTheory.Prime
import Data.List

{-
    cousins: returns pairs of cousin primes [(p,p+4),(3,7),(7,11)...]
-}

cousins = map (\x -> (x,x+4)) $ filter (\x -> isPrime $ x+4) primes
