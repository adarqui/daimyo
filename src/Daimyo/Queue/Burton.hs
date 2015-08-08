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
-- >>> size (fromList [1,2,3,4] :: Queue Int) :: Int
-- 4
--
size :: Queue a -> Int
size (Queue ([], []))         = 0
size (Queue ([], (_:xs)))     = 1 + size (Queue ([], xs))
size (Queue ((_:xs), []))     = 1 + size (Queue (xs, []))
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
front (Queue (_, (y:_))) = Just y

-- | front'
--
-- dangerous: assumes queue is not empty
--
front' :: Queue a -> a
front' = fromJust . front

-- | fromList
--
-- >>> fromList [1,2,3,4] :: Queue Int
-- Node 1 (Node 2 (Node 3 (Node 4 Empty)))
--
fromList :: [a] -> Queue a
fromList = foldr enqueue empty

-- | toList
--
-- >>> toList (fromList [1,2,3,4] :: Queue Int)
-- [1,2,3,4]
--
toList :: Queue a -> [a]
toList q = go q
  where
  go q'
    | isEmpty q' = []
    | otherwise = front' q' : go (dequeue' q')
