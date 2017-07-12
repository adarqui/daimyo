module Daimyo.Math.Mean.Generalized (
  generalizedMean,
  generalized,
  fMean
) where

-- | generalizedMean
--
generalizedMean :: Double -> [Double] -> Double
generalizedMean m l = (sum' / fromIntegral n)**(1/m)
  where
    n    = length l
    sum' = foldl (\acc y -> acc + (y**m)) 0 l

-- | generalized
--
generalized :: Double -> [Double] -> Double
generalized = generalizedMean

-- | fMean
--
fMean :: (Fractional a, Foldable t) => (a -> a) -> t a -> a
fMean f l = f $ sum' / fromIntegral n
  where
    n    = length l
    sum' = foldl (\acc y -> acc + f y) 0 l
