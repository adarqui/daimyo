module Daimyo.NumberTheory.Binet (
    binet
) where

import Daimyo.NumberTheory.GoldenRatio

{-
    The binet formula produces fibonacci numbers
-}

binet n = ((golden**n) - ((-golden)**(-n))) / (sqrt 5)
