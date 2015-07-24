module Daimyo.Sorting.HeapSort.Tree (
  hsort
) where

import           Daimyo.Queue.PriorityQueue.LeftistHeap

-- | hsort
--
hsort :: Ord a => [a] -> [a]
hsort xs = go (foldr enqueue empty xs)
  where
    go pq
      | isEmpty pq = []
      | otherwise  = front pq : go (dequeue pq)
