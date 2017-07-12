module Daimyo.Sorting.QuickSort.Naive (
  qsort,
  qsort1,
  qsort2
) where

-- | qsort
--
-- >>> qsort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
qsort :: Ord a => [a] -> [a]
qsort = go []
  where
    go acc []           = acc
    go acc (pivot:rest) = go (pivot : (go acc upper)) lower
      where
        lower = [ x | x <- rest, x <= pivot ]
        upper = [ x | x <- rest, x > pivot  ]

-- | qsort1
--
-- >>> qsort1 [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
qsort1 :: Ord a => [a] -> [a]
qsort1 []           = []
qsort1 (pivot:rest) = lh ++ middle ++ rh
  where
    lh = qsort1 [ x | x <- rest, x <= pivot]
    middle = [pivot]
    rh = qsort1 [ x | x <- rest, x > pivot ]

-- | qsort2
--
-- >>> qsort2 [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
qsort2 :: Ord a => [a] -> [a]
qsort2 []           = []
qsort2 (pivot:rest) =
  qsort2 [ x | x <- rest, x <= pivot] ++ [pivot] ++ qsort2 [ x | x <- rest, x > pivot ]
