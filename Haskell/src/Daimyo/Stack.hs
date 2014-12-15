module Daimyo.Stack (
 Stack,
 empty,
 size,
 push,
 pop,
 top,
 fromList,
 toList
) where

data Stack a = Empty | Node a (Stack a) deriving (Show, Eq)

empty :: Stack a
empty = Empty

size :: Num a => Stack b -> a
size Empty = 0
size (Node a r) = 1 + size r

push :: a -> Stack a -> Stack a
push a Empty = Node a Empty
push a (Node a' r) = Node a (Node a' r)

pop :: Stack a -> Stack a
pop Empty = error "pop on an empty stack"
pop (Node a r) = r

top :: Stack a -> a
top Empty = error "top on an empty stack"
top (Node a r) = a

fromList :: [a] -> Stack a
fromList = fromList' empty

fromList' :: Stack a -> [a] -> Stack a
fromList' st [] = st
fromList' st (x:xs) = let st' = push x st in fromList' st' xs

toList :: Stack a -> [a]
toList Empty = []
toList (Node a r) = a : toList r

t1 = empty
t2 = pop $ pop $ pop $ fromList [1..10]
