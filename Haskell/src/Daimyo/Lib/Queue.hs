module Daimyo.Lib.Queue (
 Queue,
 empty,
 size,
 enqueue,
 dequeue,
 bottom,
 fromList,
 toList
) where

data Queue a = Empty | Node a (Queue a) deriving (Show, Eq)

empty :: Queue a
empty = Empty

size :: (Num b) => Queue a -> b
size Empty = 0
size (Node a r) = 1 + size r

enqueue :: a -> Queue a -> Queue a
enqueue a Empty = Node a Empty
enqueue a (Node a' r') = Node a' (enqueue a r')

dequeue :: Queue a -> Queue a
dequeue Empty = error "dequeue on an empty queue"
dequeue (Node a r) = r

bottom :: Queue a -> a
bottom Empty = error "bottom on an empty queue"
bottom (Node a r) = a

fromList :: [a] -> Queue a
fromList = fromList' Empty

fromList' :: Queue a -> [a] -> Queue a
fromList' q [] = q
fromList' q (x:xs) = let q' = enqueue x q in fromList' q' xs

toList :: Queue a -> [a]
toList Empty = []
toList (Node a r) = a : toList r

t1 = enqueue 1 (enqueue 2 (enqueue 3 Empty))
t2 = fromList [1..100]
