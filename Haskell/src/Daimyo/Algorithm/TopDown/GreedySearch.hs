module Daimyo.Algorithm.TopDown.GreedySearch (
  searchGreedy
) where

import qualified Daimyo.Queue.PriorityQueue.List as PQ

type Successor n = n -> [n]
type Goal n      = n -> Bool
type Initial n   = n
type Solution n  = [n]

-- | searchGreedy
--
searchGreedy :: Ord node => Successor node -> Goal node -> Initial node -> Solution node
searchGreedy successor goal initial = go (PQ.enqueue initial PQ.empty)
  where
    go q
      | PQ.isEmpty q = []
      | goal (PQ.front' q) = [PQ.front' q]
      | otherwise = go (foldr PQ.enqueue PQ.empty (successor (PQ.front' q)))
