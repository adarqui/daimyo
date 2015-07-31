module Daimyo.Athletics.Run (
  milesToKilometers,
  kilometersToMiles,
  minsecToSec,
  secToMinsec
) where

-- | milesToKilometers
--
-- >>> milesToKilometers 3.4
-- 5.471756
--
milesToKilometers :: Double -> Double
milesToKilometers mi = mi * 1.60934

-- | kilometersToMiles
--
-- >>> kilometersToMiles 5.471756
-- 3.3999904974760002
--
kilometersToMiles :: Double -> Double
kilometersToMiles km = km * 0.621371

-- | minsecToSec
--
minsecToSec :: (Double, Double) -> Double
minsecToSec (minutes,seconds) = minutes*60 + seconds

-- | secToMinsec
--
secToMinsec :: Integral a => a -> (a, a)
secToMinsec s = quotRem s 60
