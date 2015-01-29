module Daimyo.NumberTheory.Log.Gap (
    gapsBase
) where

import Daimyo.NumberTheory.Log

gapsBase b x y by = map (\(x,y) -> y - x) $ logBasePairs b x y by

t_gapsBase'10 = gapsBase 10 1 10 1
