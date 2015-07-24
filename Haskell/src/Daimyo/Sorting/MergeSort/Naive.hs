module Daimyo.Sorting.MergeSort.Naive (
  msort
) where

-- | msort
--
-- >>> msort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
-- >>> msort [1,3,1,9,4,2,3,0,4,5,10,0,0,7,6] :: [Int]
-- [0,0,0,1,1,2,3,3,4,4,5,6,7,9,10]
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
    merge a@(y:ys) b@(z:zs)
      | y < z     = y : merge ys b
      | otherwise = z : merge a zs
