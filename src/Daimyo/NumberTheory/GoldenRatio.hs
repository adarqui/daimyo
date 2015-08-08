module Daimyo.NumberTheory.GoldenRatio (
    golden,
    golden'continuedFraction,
    golden'continuedFraction'abbreviatedForm
) where

import Daimyo.NumberTheory.ContinuedFraction
import Daimyo.NumberTheory.ContinuedFraction.Constant

golden = (1 + (sqrt 5)) / 2


{- continued fraction style: minimal precision -}
golden'continuedFraction = _goldenRatio


-- e continued fraction, abbreviatedForm
golden'continuedFraction'abbreviatedForm precision = (1, replicate precision 1)
