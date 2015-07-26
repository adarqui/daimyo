module Daimyo.Sorting.MergeSort.DivideAndConquer (
  msort
) where

import           Daimyo.Algorithm.TopDown.DivideAndConquer
import qualified Daimyo.Sorting.MergeSort.Naive            as M

-- | msort
--
msort :: Ord a => [a] -> [a]
msort xs = divideAndConquer ind solve divide combine xs
  where
    ind xs    = length xs <= 1
    solve     = id
    divide xs = [take n xs, drop n xs]
      where n = length xs `div` 2
    combine _ (l1:l2:[]) = M.merge l1 l2
