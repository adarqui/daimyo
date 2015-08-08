module Daimyo.Sorting.InsertionSort.Fold (
  isort,
  insert
) where

-- | isort
--
-- >>> isort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

-- | insert
--
-- >>> insert 3 [1,2,4] :: [Int]
-- [1,2,3,4]
--
insert :: Ord a => a -> [a] -> [a]
insert key [] = [key]
insert key l@(x:xs)
  | key <= x  = key : l
  | otherwise = x : insert key xs
