module Daimyo.Sorting.QuickSort.Naive (
  qsort
) where

-- | qsort
--
-- >>> qsort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (pivot:rest) =
  qsort [ x | x <- rest, x <= pivot] ++ [pivot] ++ qsort [ x | x <- rest, x > pivot ]
