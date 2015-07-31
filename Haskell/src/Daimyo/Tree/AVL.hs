{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Daimyo.Tree.AVL (
  AVLTree (..),
  empty,
  height,
  rotateLeft,
  rotateRight,
  doubleRotateRightLeft,
  doubleRotateLeftRight,
  insert,
  inOrder,
  preOrder,
  postOrder,
  fromList,
  toList
) where

-- | AVLTree
--
data AVLTree a
  = AVLEmpty
  | AVLNode a (AVLTree a) (AVLTree a)
  deriving (Eq, Ord, Show)

-- | empty
--
empty :: AVLTree a
empty = AVLEmpty

-- | height
--
height :: AVLTree a -> Int
height AVLEmpty = 0
height (AVLNode _ lf rt) = 1 + max (height lf) (height rt)

-- | rotateLeft
--
rotateLeft :: AVLTree a -> AVLTree a
rotateLeft AVLEmpty = AVLEmpty
rotateLeft (AVLNode v (AVLNode lv lflf lfrt) rt)
  = AVLNode lv lflf (AVLNode v lfrt rt)

-- | rotateRight
--
rotateRight :: AVLTree a -> AVLTree a
rotateRight AVLEmpty = AVLEmpty
rotateRight (AVLNode v lf (AVLNode rv rtlf rtrt))
  = AVLNode rv (AVLNode v lf rtlf) rtrt

-- | doubleRotateLeft
--
doubleRotateRightLeft :: AVLTree a -> AVLTree a
doubleRotateRightLeft AVLEmpty = AVLEmpty
doubleRotateRightLeft (AVLNode v lf (AVLNode rv (AVLNode rtlv rtlflf rtlfrt) rtrt))
  = AVLNode rtlv (AVLNode v lf rtlflf) (AVLNode rv rtlfrt rtrt)

-- | doubleRotateRight
--
doubleRotateLeftRight :: AVLTree a -> AVLTree a
doubleRotateLeftRight AVLEmpty = AVLEmpty
doubleRotateLeftRight (AVLNode v (AVLNode lv lflf (AVLNode lfrv lfrtlf lfrtrt)) rt)
  = AVLNode lfrv (AVLNode lv lflf lfrtlf) (AVLNode v lfrtrt rt)

-- | insert
--
insert :: Ord a => a -> AVLTree a -> AVLTree a
insert x AVLEmpty = AVLNode x AVLEmpty AVLEmpty
insert x t@(AVLNode v lf rt)
  | x < v =
    if ((height newlf - height rt) == 2)
       then if x < newlfv
          then rotateLeft (AVLNode v newlf rt)
          else doubleRotateLeftRight (AVLNode v newlf rt)
       else (AVLNode v newlf rt)
  | x == v = t
  | otherwise =
    if ((height newrt - height lf) == 2)
       then if x > newrtv
          then rotateRight (AVLNode v lf newrt)
          else doubleRotateRightLeft (AVLNode v lf newrt)
       else (AVLNode v lf newrt)
  where
      newlf@(AVLNode newlfv _ _) = insert x lf
      newrt@(AVLNode newrtv _ _) = insert x rt

-- | fromList
--
fromList :: Ord a => [a] -> AVLTree a
fromList = foldr insert AVLEmpty

-- | toList
--
-- >>> toList (fromList [9,0,2,1,0,4,8] :: AVLTree Int)
-- [0,1,2,4,8,9]
--
toList :: AVLTree a -> [a]
toList = inOrder

-- | preOrder
--
-- >>> preOrder (fromList [9,0,2,1,0,4,8] :: AVLTree Int)
-- [4,1,0,2,8,9]
--
preOrder :: AVLTree a -> [a]
preOrder AVLEmpty = []
preOrder (AVLNode v l r) = v : preOrder l ++ preOrder r

-- | inOrder
--
-- >>> inOrder (fromList [9,0,2,1,0,4,8] :: AVLTree Int)
-- [0,1,2,4,8,9]
--
inOrder :: AVLTree a -> [a]
inOrder AVLEmpty = []
inOrder (AVLNode v l r) = inOrder l ++ [v] ++ inOrder r

-- | postOrder
--
postOrder :: AVLTree a -> [a]
postOrder AVLEmpty = []
postOrder (AVLNode v l r) = postOrder l ++ postOrder r ++ [v]
