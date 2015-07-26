module Daimyo.Sorting.InsertionSort.DivideAndConquer (
  isort
) where

import           Daimyo.Algorithm.TopDown.DivideAndConquer
import           Data.List

-- | isort
--
-- >>> isort [1,3,5,2,0,9,4,7] :: [Int]
-- [0,1,2,3,4,5,7,9]
--
isort :: Ord a => [a] -> [a]
isort xs = divideAndConquer ind solve divide combine xs
  where
    ind xs        = length xs <= 1
    solve         = id
    divide (x:xs) = [takeWhile (<= x) xs, dropWhile (<= x) xs]
    combine (x:_) (l1:l2:[]) = insert x (l1 ++ l2)
