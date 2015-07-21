module Daimyo.Tree.BaseTree (
  Tree (..),
  fromList,
  toList,
  leaf,
  size,
  preOrder,
  inOrder,
  postOrder,
  find,
  insert,
  remove,
  removeSubTree,
  pp
) where

-- | Tree
--
-- >>> Node True Empty Empty
-- Node True Empty Empty
--
-- >>> Node True (Node False Empty Empty) Empty
-- Node True (Node False Empty Empty) Empty
--
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- | toList
--
-- >>> toList $ Node True (Node False Empty Empty) (Node True Empty Empty)
-- [True,False,True]
--
toList :: Tree a -> [a]
toList = preOrder

-- | leaf
--
-- >>> Leaf True
-- Node True Empty Empty
--
leaf :: a -> Tree a
leaf a = Node a Empty Empty

-- | size
--
-- >>> size (Node True (Node False Empty Empty) (Node True Empty Empty)) :: Int
-- 3
--
size :: Num a => Tree b -> a
size Empty        = 0
size (Node _ l r) = 1 + size l + size r

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

-- | postOrder
--
-- >>> postOrder (fromList [1,5,2,7,3,8] :: Tree Int)
-- [1,2,5,7,3,8]
--
postOrder :: Tree a -> [a]
postOrder Empty        = []
postOrder (Node a l r) = postOrder l ++ postOrder r ++ [a]

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
 | e < a = Node a (insert e l) r
 | e > a = Node a l (insert e r)

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
removeSubTree e t@(Node a l r)
 | e == a = Empty
 | e < a  = Node a (removeSubTree e l) r
 | e > a  = Node a l (removeSubTree e r)

-- | pp Pretty Printer
--
pp :: Show a => Tree a -> IO ()
pp = (mapM_ putStrLn) . treeIndent
 where
  treeIndent Empty          = ["-- /-"]
  treeIndent (Node v lb rb) =
   ["--" ++ (show v)] ++
   map ("  |" ++) ls ++
   ("  `" ++ r) : map ("   " ++) rs
    where
     (r:rs) = treeIndent $ rb
     ls     = treeIndent $ lb
