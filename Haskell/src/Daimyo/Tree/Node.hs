-- A binary tree with nodes that contain keys only, no values
module Daimyo.Tree.Node (
 Tree (..),
 fromList,
 toList,
 leaf,
 size,
 preOrder,
 inOrder,
 postOrder,
 member,
 insert,
 fold,
-- union,
-- intersection,
-- difference,
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

member :: (Eq a) => a -> Tree a -> Bool
member _ Empty = False
member e (Node a l r) =
 if e == a
  then True
  else
   case (member e l) of
    True -> True
    False -> member e r

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

fold :: (Tree b -> a -> Tree b) -> Tree b -> Tree a -> Tree b
fold _ acc Empty = Empty
--fold f acc (Node a l r) = let k = f acc a in foldNode a l r

foldl        :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs


union :: (Ord a) => Tree a -> Tree a -> Tree a
union m n = fold (\acc e -> insert e acc) m n

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

t1 = insert 9 $ insert 0 $ fromList [1..8]
t2 = inOrder $ remove'Subtree 5 $ remove 1 $ remove 10 t1
