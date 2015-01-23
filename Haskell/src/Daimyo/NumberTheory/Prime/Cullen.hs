module Daimyo.NumberTheory.Prime.Cullen (
    cullens,
    isCullen
) where

import Daimyo.NumberTheory.Prime

cullens = [ p | p <- [2..], isCullen p ]

isCullen n = isPrime $ n * (2^n) + 1
