module Daimyo.Algorithm.TopDown.PriorityFirstSearch (
  searchPFS
) where

import qualified Daimyo.Queue.PriorityQueue.List as PQ

type Successor n = n -> [n]
type Goal n      = n -> Bool
type Initial n   = n
type Solution n  = [n]

-- | searchPFS
--
searchPFS :: Ord node => Successor node -> Goal node -> Initial node -> Solution node
searchPFS successor goal initial = go (PQ.enqueue initial PQ.empty)
  where
    go q
      | PQ.isEmpty q = []
      | goal (PQ.front' q) = PQ.front' q : go (PQ.dequeue' q)
      | otherwise = go (foldr PQ.enqueue (PQ.dequeue' q) (successor (PQ.front' q)))
