module Daimyo.Sorting.MergeSort.Naive (
  msort
) where

-- | msort
--
-- >>> msort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort xs1) (msort xs2)
  where
    xs1 = take k xs
    xs2 = drop k xs
    k   = length xs `div` 2
    merge [] zs = zs
    merge ys [] = ys
    merge (y:ys) (z:zs)
      | y < z     = y : z : merge ys zs
      | otherwise = z : y : merge ys zs
