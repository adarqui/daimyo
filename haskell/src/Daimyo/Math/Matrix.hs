{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Daimyo.Math.Matrix (
  new
) where



import Data.List



data Matrix = forall a. (Eq a, Ord a, Num a) => Matrix {
  rows :: Int,
  cols :: Int,
  size :: Int,
  entries :: [a]
}



new :: Int -> Int -> [Int] -> Matrix
new rows cols entries = Matrix {
  rows = rows,
  cols = cols,
  size = rows * cols,
  entries = entries
}



nth_offset :: Matrix -> Int -> Int -> Int
nth_offset Matrix{..} row col = (((row-1) * cols) + col) - 1



-- nth :: forall a. Matrix -> Int -> Int -> a
nth m@Matrix{..} row col =
  entries !! offset
  where
  offset = nth_offset m row col



-- | detExperiment
--
-- Experimental function to find 
