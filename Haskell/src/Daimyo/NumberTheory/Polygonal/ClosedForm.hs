module Daimyo.NumberTheory.Polygonal.ClosedForm (
  polygonal
) where

-- | polygonal
--
-- >>> polygonal 5 :: Int
-- 35
--
polygonal :: Integral a => a -> a
polygonal n = (n*(3*n - 1)) `div` 2
