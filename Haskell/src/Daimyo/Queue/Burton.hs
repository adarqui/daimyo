module Daimyo.Queue.Burton (
  Queue,
  size,
  enqueue,
  dequeue,
  dequeue',
  front,
  front',
  empty,
  isEmpty,
  fromList,
  toList
) where

import           Data.Maybe

-- | Queue
--
newtype Queue a
  = Queue ([a], [a])
  deriving (Show)

-- | empty
--
empty :: Queue a
empty = Queue ([], [])

-- | isEmpty
--
isEmpty :: Queue a -> Bool
isEmpty (Queue ([], [])) = True
isEmpty (Queue _)        = False

-- | size
--
size :: Queue a -> Int
size (Queue ([], []))         = 0
size (Queue ([], (_:xs)))     = 1 + size (Queue ([], xs))
size (Queue ((_:xs), (_:ys))) = 1 + size (Queue (xs, ys))

-- | enqueue
--
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue ([], [])) = Queue ([x], [])
enqueue x (Queue (xs, ys)) = Queue (xs, x:ys)

-- | dequeue
--
dequeue :: Queue a -> Maybe (Queue a)
dequeue (Queue ([], []))     = Nothing
dequeue (Queue ([], ys))     = Just $ Queue (tail (reverse ys), [])
dequeue (Queue ((_:xs), ys)) = Just $ Queue (xs, ys)

-- | dequeue'
--
-- dangerous: assumes queue is not empty
--
dequeue' :: Queue a -> Queue a
dequeue' = fromJust . dequeue

-- | front
--
front :: Queue a -> Maybe a
front (Queue ([], []))   = Nothing
front (Queue ((x:_), _)) = Just x

-- | front'
--
-- dangerous: assumes queue is not empty
--
front' :: Queue a -> a
front' = fromJust . front

-- | fromList
--
fromList = undefined

-- | toList
--
toList = undefined
