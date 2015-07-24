module Daimyo.Sorting.InsertionSort.Naive (
  isort
) where

-- | isort
--
-- >>> isort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert (isort xs)
  where
    insert ys = takeWhile (<= x) ys ++ [x] ++ dropWhile (<= x) ys
