module Daimyo.Queue.Naive (
  Queue,
  empty,
  size,
  enqueue,
  dequeue,
  front,
  fromList,
  toList
) where

-- | Queue
--
data Queue a
  = Empty
  | Node a (Queue a)
  deriving (Show, Eq)

-- | empty
--
empty :: Queue a
empty = Empty

-- | size
--
size :: (Num b) => Queue a -> b
size Empty      = 0
size (Node _ r) = 1 + size r

-- | enqueue
--
enqueue :: a -> Queue a -> Queue a
enqueue a Empty        = Node a Empty
enqueue a (Node a' r') = Node a' (enqueue a r')

-- | dequeue
--
dequeue :: Queue a -> Maybe (Queue a)
dequeue Empty      = Nothing
dequeue (Node _ r) = Just r

-- | front
--
front :: Queue a -> Maybe a
front Empty      = Nothing
front (Node a _) = Just a

-- | fromList
--
fromList :: [a] -> Queue a
fromList = go Empty
  where
    go q []     = q
    go q (x:xs) = go (enqueue x q) xs

-- | toList
--
toList :: Queue a -> [a]
toList Empty      = []
toList (Node a r) = a : toList r
