{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Daimyo.Math.Matrix (
    new
  , det_experiment
) where



import Data.List



data Matrix a = Num a => Matrix {
  rows :: Int,
  cols :: Int,
  size :: Int,
  entries :: [a]
}

deriving instance Show a => Show (Matrix a)



new :: Num a => Int -> Int -> [a] -> Matrix a
new rows cols entries = Matrix {
  rows = rows,
  cols = cols,
  size = rows * cols,
  entries = entries
}



nth_offset :: Matrix a -> Int -> Int -> Int
nth_offset Matrix{..} row col = (((row-1) * cols) + col) - 1



nth :: Num a => Matrix a -> Int -> Int -> a
nth m@Matrix{..} row col =
  entries !! offset
  where
  offset = nth_offset m row col



-- | det_experiment
--
-- Experimental function to find 
--
---
-- det_NxN()
--
-- Definition 1.5 of Cryptography Theory & Practice:
--
-- Suppose that A = (a_ij) is an m x m matrix.
--  For
--   1 <= i <= m,
--   1 <= j <= m,
--  Define A_ij to be the matrix obtained from A by:
--   deleting the ith row
--   deleting the jth column
--
--  The determinant of A, denoted det_A, is:
--   the value a_1,1 if m = 1.
--   if m > 1, then det_A is computed recursively from the formula:
--    det A = SUM(j=1 to m) of ((-1)^i+j)a_i,j(det A_ij),
--     where i is any fixed integer between 1 and m.
--
det_experiment :: Num a => Matrix a -> Maybe a
det_experiment m@Matrix{..} =
  case (rows, cols) of
    (1,1) -> Just $ nth m 1 1
    _     -> Nothing
