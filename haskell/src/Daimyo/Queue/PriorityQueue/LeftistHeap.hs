module Daimyo.Queue.PriorityQueue.LeftistHeap (
  PQueue,
  empty,
  isEmpty,
  enqueue,
  front,
  dequeue
) where

import qualified Daimyo.Heap.Leftist as H

-- | PQueue
--
newtype PQueue a
  = PQueue (H.Heap a)
  deriving (Eq, Ord, Show)

-- | empty
--
empty :: Ord a => PQueue a
empty = PQueue H.empty

-- | isEmpty
--
isEmpty :: (Ord a) => PQueue a -> Bool
isEmpty (PQueue heap) = H.isEmpty heap

-- | enqueue
--
enqueue :: Ord a =>  a -> PQueue a -> PQueue a
enqueue x (PQueue heap) = PQueue (H.insert x heap)

-- | front
--
front :: Ord a => PQueue a -> a
front (PQueue heap) = H.find heap

-- | dequeue
--
dequeue :: Ord a => PQueue a -> PQueue a
dequeue (PQueue heap) = PQueue (H.delete heap)
