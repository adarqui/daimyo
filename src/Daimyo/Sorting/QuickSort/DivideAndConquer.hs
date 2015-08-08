{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Daimyo.Sorting.QuickSort.DivideAndConquer (
  qsort
) where

import           Daimyo.Algorithm.TopDown.DivideAndConquer

-- | qsort
--
-- >>> qsort [1,3,5,2,0,9,4,7] :: [Int]
-- [0,1,2,3,4,5,7,9]
--
qsort :: Ord a => [a] -> [a]
qsort l = divideAndConquer ind solve divide combine l
  where
    ind xs        = length xs <= 1
    solve         = id
    divide (x:xs) = [ [y|y<-xs, y<=x], [y|y<-xs, y>x] ]
    combine (x:_) (l1:l2:[]) = l1 ++ [x] ++ l2
