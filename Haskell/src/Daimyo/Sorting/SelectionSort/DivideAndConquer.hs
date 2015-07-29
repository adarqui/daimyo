{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Daimyo.Sorting.SelectionSort.DivideAndConquer (
  ssort
) where

import           Daimyo.Algorithm.TopDown.DivideAndConquer
import           Data.List

-- | ssort
--
-- >>> ssort [1,3,5,2,0,9,4,7] :: [Int]
-- [0,1,2,3,4,5,7,9]
--
ssort :: Ord a => [a] -> [a]
ssort l = divideAndConquer ind solve divide combine l
  where
    ind xs    = length xs <= 1
    solve     = id
    divide xs = [ [m], delete m xs ]
      where m = minimum xs
    combine _ (l1:l2:[]) = l1 ++ l2
