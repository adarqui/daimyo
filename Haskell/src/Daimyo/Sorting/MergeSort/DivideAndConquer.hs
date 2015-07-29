{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Daimyo.Sorting.MergeSort.DivideAndConquer (
  msort,
  msortReverse
) where

import           Daimyo.Algorithm.TopDown.DivideAndConquer
import qualified Daimyo.Sorting.MergeSort.Naive            as M

-- | msort
--
-- >>> msort [1,3,5,2,0,9,4,7] :: [Int]
-- [0,1,2,3,4,5,7,9]
--
msort :: Ord a => [a] -> [a]
msort l = divideAndConquer ind solve divide combine l
  where
    ind xs    = length xs <= 1
    solve     = id
    divide xs = [take n xs, drop n xs]
      where n = length xs `div` 2
    combine _ (l1:l2:[]) = M.merge l1 l2

-- | msortReverse
--
-- >>> msortReverse [1,3,5,2,0,9,4,7] :: [Int]
-- [7,4,9,3,1,5,2,0]
--
msortReverse :: Ord a => [a] -> [a]
msortReverse l = divideAndConquer ind solve divide combine l
  where
    ind xs    = length xs <= 1
    solve     = id
    divide xs = [take n xs, drop n xs]
      where n = length xs `div` 2
    combine _ (l1:l2:[]) = reverse $ M.merge l1 l2
