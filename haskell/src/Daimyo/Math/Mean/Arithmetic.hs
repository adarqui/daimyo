module Daimyo.Math.Mean.Arithmetic (
  arithmeticMean,
  arithmetic
) where

import Data.List

-- | arithmeticMean
--
-- >>> arithmeticMean [4,36,45,50,75]
-- ...
--
-- >>> arithmeticMean [3,3,3]
-- ...
--
-- >>> arithmeticMean [1,2,3]
-- ...
--
arithmeticMean :: Integral a => [a] -> Double
arithmeticMean l = fromIntegral sum' / fromIntegral n
  where
    n    = length l
    sum' = foldl' (+) 0 l

-- | arithmetic
--
arithmetic :: Integral a => [a] -> Double
arithmetic = arithmeticMean
