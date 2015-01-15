module Daimyo.NumberTheory.Constants.Brun (
    brun
) where

import Daimyo.NumberTheory.Prime.Twin

brun precision = sum $ brun'values precision

brun'values precision = concatMap (\(x,y) -> 1/(fromIntegral x :: Double) : 1/(fromIntegral y :: Double) : []) $ take precision twins

brun'const = 1.902160582582
