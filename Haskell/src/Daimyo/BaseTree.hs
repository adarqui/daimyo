module Daimyo.Tree (
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
 pp
) where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

toList = preOrder

leaf :: a -> Tree a
leaf a = Node a Empty Empty

size :: (Num a) => Tree b -> a
size Empty = 0
size (Node a l r) = 1 + size l + size r

fromList l = fromList' l Empty

fromList' [] m = m
fromList' (x:xs) m = insert x (fromList' xs m)
{-
fromList' [] = Empty
--fromList (x:xs) = Node x (fromList lefts) (fromList rights)
fromList' (x:xs) m = insert x (fromList' lefts m) (fromList' rights m)
 where
  lefts = takeWhile (<= x) xs
  rights = dropWhile (<= x) xs
-}

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a l r) = a : preOrder l ++ preOrder r

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a l r) = inOrder l ++ [a] ++ inOrder r

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a l r) = postOrder l ++ postOrder r ++ [a]

levelOrder :: Tree a -> [a]
levelOrder (Node a l r) = []

find :: (Eq a) => a -> Tree a -> Bool
find _ Empty = False
find e (Node a l r) =
 if e == a
  then True
  else
   case (find e l) of
    True -> True
    False -> find e r

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert e Empty = Node e Empty Empty
insert e t@(Node a l r)
 | e == a = t
 | e < a = Node a (insert e l) r
 | e > a = Node a l (insert e r)

remove :: (Eq a, Ord a) => a -> Tree a -> Tree a
remove e Empty = Empty
remove e (Node a l r)
 | e == a = Empty
 | e < a = Node a (remove e l) r
 | e > a = Node a l (remove e r)

remove'Subtree :: (Eq a, Ord a) => a -> Tree a -> Tree a
remove'Subtree e Empty = Empty
remove'Subtree e t@(Node a l r)
 | e == a = Empty
 | e < a = Node a (remove'Subtree e l) r
 | e > a = Node a l (remove'Subtree e r)

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

t1 = inOrder $ remove'Subtree 5 $ remove 1 $ insert 10 $ insert 9 $ insert 0 $ fromList [1,2,4,5,6,7,8]
