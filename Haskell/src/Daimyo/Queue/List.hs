module Daimyo.Queue.List (
  Queue,
  size,
  enqueue,
  dequeue,
  bottom,
  empty,
  isEmpty,
  fromList,
  toList
) where

-- | Queue
--
newtype Queue a
  = Queue [a]
  deriving (Show)

-- | empty
--
empty :: Queue a
empty = Queue []

-- | isEmpty
--
isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty (Queue _)  = False

-- | size
--
size :: Queue a -> Int
size (Queue [])     = 0
size (Queue (_:xs)) = size (Queue xs)

-- | enqueue
--
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

-- | dequeue
--
dequeue :: Queue a -> Maybe (Queue a)
dequeue (Queue [])     = Nothing
dequeue (Queue (_:xs)) = Just $ Queue xs

-- | bottom
--
bottom :: Queue a -> Maybe a
bottom (Queue [])    = Nothing
bottom (Queue (x:_)) = Just x

-- | fromList
--
fromList = undefined

-- | toList
--
toList = undefined
