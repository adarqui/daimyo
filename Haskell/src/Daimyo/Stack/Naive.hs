module Daimyo.Stack.Naive (
  Stack,
  empty,
  size,
  push,
  pop,
  top,
  fromList,
  toList
) where

-- | Stack
--
data Stack a
  = Empty
  | Node a (Stack a)
  deriving (Show, Eq)

-- | empty
--
empty :: Stack a
empty = Empty

-- | size
--
-- >>> size (fromList [1..5] :: Stack Int) :: Integer
-- 5
--
size :: Num a => Stack b -> a
size Empty = 0
size (Node a r) = 1 + size r

-- | push
--
-- >>> push 6 (fromList [1..5] :: Stack Int)
-- Node 6 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty)))))
--
push :: a -> Stack a -> Stack a
push a Empty = Node a Empty
push a (Node a' r) = Node a (Node a' r)

-- | pop
--
-- >>> pop $ pop $ pop $ fromList [1..5] :: Stack Int
-- Node 2 (Node 1 Empty)
--
pop :: Stack a -> Stack a
pop Empty = error "pop on an empty stack"
pop (Node a r) = r

-- | top
--
-- >>> top $ fromList [False, True]
-- True
--
top :: Stack a -> a
top Empty = error "top on an empty stack"
top (Node a r) = a

-- | fromList
--
-- >>> fromList [False, True]
-- Node True (Node False Empty)
--
fromList :: [a] -> Stack a
fromList = go empty
  where
    go st [] = st
    go st (x:xs) = go (push x st) xs

-- | toList
--
-- >>> toList (fromList [1..5] :: Stack Int)
-- [5,4,3,2,1]
--
toList :: Stack a -> [a]
toList Empty = []
toList (Node a r) = a : toList r
