-- mostly taken from AAFA

module Daimyo.Heap.Leftist (
  Heap,
  empty,
  isEmpty,
  find,
  insert,
  delete,
  createHeap,
  rank,
  merge
) where

-- | Heap
--
data Heap a
  = Empty
  | Heap a Int (Heap a) (Heap a)
  deriving (Eq, Ord, Show)

-- | empty
--
empty :: Ord a => Heap a
empty = Empty

-- | isEmpty
--
isEmpty :: Ord a => Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | find
--
-- find $ insert 2 $ insert 6 $ insert 0 $ insert 5 $ insert 1 (empty :: Heap Int)
-- 0
--
find :: Ord a => Heap a -> a
find Empty          = error "empty"
find (Heap x _ a b) = x

-- | rank
--
-- >>> rank $ insert 2 $ insert 6 $ insert 0 $ insert 5 $ insert 1 (empty :: Heap Int)
-- 2
--
rank :: Ord a => Heap a -> Int
rank Empty          = 0
rank (Heap _ r _ _) = r

-- | createHeap
createHeap :: Ord a => a -> Heap a -> Heap a -> Heap a
createHeap x a b
  | rank a >= rank b = Heap x (rank b + 1) a b
  | otherwise        = Heap x (rank a + 1) b a

-- | merge
--
-- >>> merge (insert 2 $ insert 3 $ insert 1 (empty :: Heap Int)) (insert 9 $ insert 7 $ insert 8 (empty :: Heap Int))
-- Heap 1 2 (Heap 3 1 Empty Empty) (Heap 2 1 (Heap 7 2 (Heap 8 1 Empty Empty) (Heap 9 1 Empty Empty)) Empty)
--
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Empty = h
merge Empty h = h
merge h1@(Heap x _ a1 b1) h2@(Heap y _ a2 b2)
  | x <= y    = createHeap x a1 (merge b1 h2)
  | otherwise = createHeap y a2 (merge h1 b2)

-- | insert
--
-- >>> insert 2 $ insert 6 $ insert 0 $ insert 5 $ insert 1 (empty :: Heap Int)
-- Heap 0 2 (Heap 1 1 (Heap 5 1 Empty Empty) Empty) (Heap 2 1 (Heap 6 1 Empty Empty) Empty)
--
insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (Heap x 1 Empty Empty) h

-- | insert
--
-- >>> delete $ delete $ insert 2 $ insert 6 $ insert 0 $ insert 5 $ insert 1 (empty :: Heap Int)
-- Heap 2 2 (Heap 6 1 Empty Empty) (Heap 5 1 Empty Empty)
--
delete :: (Ord a) => Heap a -> Heap a
delete Empty          = error "empty"
delete (Heap x _ a b) = merge a b
