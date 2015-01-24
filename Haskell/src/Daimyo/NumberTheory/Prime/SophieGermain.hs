module Daimyo.NumberTheory.Prime.SophieGermain (
    isSophieGermainPrime
) where

import Daimyo.NumberTheory.Prime

isSophieGermainPrime p = isPrime p && isPrime (2*p+1)
