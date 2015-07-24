-- mostly taken from AAFA

module Daimyo.Heap.Leftist (
  Heap,
  empty,
  isEmpty,
  find,
  insert,
  delete,
  createHeap,
  merge
) where

data Heap a
  = Empty
  | Heap a Int (Heap a) (Heap a)
  deriving (Eq, Ord, Show)

empty :: Ord a => Heap a
empty = Empty

isEmpty :: Ord a => Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

find :: Ord a => Heap a -> a
find Empty          = error "empty"
find (Heap x _ a b) = x

rank :: Ord a => Heap a -> Int
rank Empty          = 0
rank (Heap _ r _ _) = r

createHeap :: Ord a => a -> Heap a -> Heap a -> Heap a
createHeap x a b
  | rank a >= rank b = Heap x (rank b + 1) a b
  | otherwise        = Heap x (rank a + 1) b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Heap x _ a1 b1) h2@(Heap y _ a2 b2)
  | x <= y    = createHeap x a1 (merge b1 h2)
  | otherwise = createHeap y a2 (merge h1 b2)

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (Heap x 1 Empty Empty) h

delete :: (Ord a) => Heap a -> Heap a
delete Empty          = error "empty"
delete (Heap x _ a b) = merge a b
