module Daimyo.Sorting.HeapSort.List (
  hsort
) where

import           Daimyo.Queue.PriorityQueue.List
import           Data.Maybe

-- | hsort
--
-- >>> hsort [1,3,1,9,4,2,3,0,4,5,10,0,0,7,6] :: [Int]
-- [0,0,0,1,1,2,3,3,4,4,5,6,7,9,10]
--
hsort :: Ord a => [a] -> [a]
hsort xs = go (foldr enqueue empty xs)
  where
    go pq
      | isEmpty pq = []
      | otherwise  = (fromJust $ front pq) : go (fromJust $ dequeue pq)
