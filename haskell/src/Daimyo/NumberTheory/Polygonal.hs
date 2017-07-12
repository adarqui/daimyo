module Daimyo.NumberTheory.Polygonal (
  polygonal
) where

-- | polygonal
--
-- >>> polygonal 5 :: Int
-- 35
--
polygonal :: Integral a => a -> a
polygonal 1 = 1
polygonal n = polygonal (n-1) + (3*(n-1)) + 1
