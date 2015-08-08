module Daimyo.NumberTheory.ContinuedFraction.Constant (
    _pi,
    _e,
    _goldenRatio,
    _goldenRatioN,
    nRatioN,
    ratiosN
) where

import Daimyo.NumberTheory.ContinuedFraction

_pi = continuedFraction (3, [7,15,1,292,1,1,1,2,1,3,1])

_e = continuedFraction (2, [1,2,1,1,4,1,1,6,1,1,8])

_goldenRatio = continuedFraction (1, [1,1,1,1,1,1,1,1,1,1,1])

_goldenRatioN precision = continuedFraction (1, replicate precision 1)

{-
    n' + 1 / (n' + (1 / ...) ...)
-}
nRatioN n' n = continuedFraction (n', replicate n n')

ratiosN n = map (\n' -> continuedFraction (n', replicate n n')) [1..]
