{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-missing-methods #-}

module Daimyo.Tree.Node (
  Tree (..),
  fromList,
  toList,
  leaf,
  size,
  depth,
  countEmpty,
  preOrder,
  inOrder,
  inOrderOptimized,
  postOrder,
  levelOrder,
  find,
  insert,
  tflip,
  tfold,
  tmap,
-- union,
-- intersection,
-- difference,
  pp
) where

import           Control.Applicative
import           Control.Monad
import           System.Random

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- | Functor
--
-- >>> fmap not $ Node True Empty Empty
-- Node False Empty Empty
--
instance Functor Tree where
  fmap f = tmap f

-- | Applicative
--
-- >>> not <$> Node False Empty Empty
-- Node True Empty Empty
--
instance Applicative Tree where
  pure a = Node a Empty Empty
  f <*> Empty = Empty
  node@(Node f _ _) <*> Node v l r = Node (f v) (node <*> l) (node <*> r)

-- | Random
--
-- should just create a list using randoms g :: [Blah] and then fromList it into a Tree
--
-- >>> take 2 (randoms (mkStdGen 5) :: [Tree Bool])
-- [Node True Empty Empty,Node True Empty Empty]
--
instance Random (Tree Bool) where
  random g = (Node a Empty Empty, g')
    where
      (a, g') = random g

-- | toList
--
-- >>> toList $ Node True (Node False Empty Empty) (Node True Empty Empty)
-- [True,False,True]
--
toList :: Tree a -> [a]
toList = preOrder

-- | leaf
--
-- >>> leaf True
-- Node True Empty Empty
--
leaf :: a -> Tree a
leaf a = Node a Empty Empty

-- | size
--
-- >>> size (Node True (Node False Empty Empty) (Node True Empty Empty)) :: Int
-- 3
size :: Num a => Tree b -> a
size Empty        = 0
size (Node _ l r) = 1 + size l + size r

-- | depth
--
-- >>> depth $ (Node 5 (Node 8 (Node 3 Empty Empty) (Node 1 Empty Empty)) (Node 6 Empty (Node 4 Empty Empty)) :: Tree Int)
--
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = max (go 1 l) (go 1 r)
  where
    go d Empty        = 0
    go d (Node _ l r) = 1 + go (d+1) l + go (d+1) r

-- | countEmpty
--
-- >>> ountEmpty $ (Node 5 (Node 8 (Node 3 Empty Empty) (Node 1 Empty Empty)) (Node 6 Empty (Node 4 Empty Empty)) :: Tree Int)
-- 7
--
countEmpty :: Tree a -> Int
countEmpty = tfoldWithNodes f 0
  where
    f acc Empty        = acc + 1
    f acc (Node _ _ _) = acc

-- | fromList
--
-- >>> fromList [1,5,2,7,3,8] :: Tree Int
-- Node 8 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 7 (Node 5 Empty Empty) Empty)) Empty
--
fromList :: Ord a => [a] -> Tree a
fromList l = go l Empty
  where
    go [] m     = m
    go (x:xs) m = insert x (go xs m)

-- | preOrder
--
-- >>> preOrder (fromList [1,5,2,7,3,8] :: Tree Int)
-- [8,3,2,1,7,5]
--
preOrder :: Tree a -> [a]
preOrder Empty        = []
preOrder (Node a l r) = a : preOrder l ++ preOrder r

-- | inOrder
--
-- >>> inOrder (fromList [1,5,2,7,3,8] :: Tree Int)
-- [1,2,3,5,7,8]
--
inOrder :: Tree a -> [a]
inOrder Empty        = []
inOrder (Node a l r) = inOrder l ++ [a] ++ inOrder r

-- | inOrderOptimized
--
-- >>> inOrderOptimized (fromList [1,5,2,7,3,8] :: Tree Int)
-- [1,2,3,5,7,8]
--
inOrderOptimized :: Tree a -> [a]
inOrderOptimized t = go t []
  where
    go Empty acc        = acc
    go (Node v l r) acc = go l (v : go r acc)

-- | postOrder
--
-- >>> postOrder (fromList [1,5,2,7,3,8] :: Tree Int)
-- [1,2,5,7,3,8]
--
postOrder :: Tree a -> [a]
postOrder Empty        = []
postOrder (Node a l r) = postOrder l ++ postOrder r ++ [a]

-- | levelOrder
--
levelOrder :: Tree a -> [a]
levelOrder (Node a l r) = []

-- | find
--
-- >>> find 7 (fromList [1,5,2,7,3,8] :: Tree Int)
-- True
--
-- >>> find 10 (fromList [1,5,2,7,3,8] :: Tree Int)
-- False
--
find :: Eq a => a -> Tree a -> Bool
find _ Empty = False
find e (Node a l r)
  | e == a    = True
  | otherwise = find e l || find e r

-- | insert
--
-- >>> insert 10 (fromList [1,5,2,7,3,8] :: Tree Int)
-- Node 8 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 7 (Node 5 Empty Empty) Empty)) (Node 10 Empty Empty)
--
insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert e Empty = Node e Empty Empty
insert e t@(Node a l r)
 | e == a = t
 | e < a  = Node a (insert e l) r
 | e > a  = Node a l (insert e r)

-- | remove
--
-- >>> remove 10 $ insert 10 (fromList [1,5,2,7,3,8] :: Tree Int)
-- Node 8 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 7 (Node 5 Empty Empty) Empty)) Empty
--
remove :: (Eq a, Ord a) => a -> Tree a -> Tree a
remove e Empty = Empty
remove e (Node a l r)
 | e == a = Empty
 | e < a  = Node a (remove e l) r
 | e > a  = Node a l (remove e r)

-- | removeSubTree
--
-- >>> inOrder $ removeSubTree 5 $ remove 1 $ insert 10 $ insert 9 $ insert 0 $ fromList [1,2,4,5,6,7,8]
-- [6,7,8,9,10]
--
removeSubTree :: (Eq a, Ord a) => a -> Tree a -> Tree a
removeSubTree e Empty = Empty
removeSubTree e (Node a l r)
 | e == a = Empty
 | e < a = Node a (removeSubTree e l) r
 | e > a = Node a l (removeSubTree e r)

-- | tflip
--
-- >>> inOrderOptimized $ tflip (fromList [1,5,2,7,3,8] :: Tree Int)
-- [8,7,5,3,2,1]
--
tflip :: Tree a -> Tree a
tflip Empty = Empty
tflip (Node v l r) = Node v (tflip r) (tflip l)

-- | tfold
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
--
-- >>> tfold (+) 0 (fromList [1..4] :: Tree Int)
-- 10
tfold :: (b -> a -> b) -> b -> Tree a -> b
tfold _ acc Empty = acc
tfold f acc (Node v l r) = f (tfold f (tfold f acc r) l) v

-- | tfold
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
--
-- >>> tfold (+) 0 (fromList [1..4] :: Tree Int)
-- 10
tfoldWithNodes :: (b -> Tree a -> b) -> b -> Tree a -> b
tfoldWithNodes f acc Empty = f acc Empty
tfoldWithNodes f acc (Node v l r) = f (tfoldWithNodes f (tfoldWithNodes f acc r) l) (Node v l r)

-- | tmap
--
-- >>> tmap show $ (fromList [1,5,7,3] :: Tree Int)
-- Node "3" (Node "1" Empty Empty) (Node "7" (Node "5" Empty Empty) Empty)
--
-- >>> tmap (\x -> x*2) (Node 5 (Node 8 (Node 3 Empty Empty) (Node 1 Empty Empty)) (Node 6 Empty (Node 4 Empty Empty)) :: Tree Int)
-- Node 10 (Node 16 (Node 6 Empty Empty) (Node 2 Empty Empty)) (Node 12 Empty (Node 8 Empty Empty))
--
tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Empty        = Empty
tmap f (Node v l r) = Node (f v) (tmap f l) (tmap f r)

-- | union
--
union :: (Ord a) => Tree a -> Tree a -> Tree a
union = undefined

pp :: Show a => Tree a -> IO ()
pp = (Prelude.mapM_ putStrLn) . treeIndent
 where
  treeIndent Empty          = ["-- /-"]
  treeIndent (Node v lb rb) =
   ["--" ++ (show v)] ++
   map ("  |" ++) ls ++
   ("  `" ++ r) : map ("   " ++) rs
    where
     (r:rs) = treeIndent $ rb
     ls     = treeIndent $ lb
