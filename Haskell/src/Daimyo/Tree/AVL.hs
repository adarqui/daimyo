module Daimyo.Tree.AVL (
  AVLTree (..),
  empty,
  height,
  rotateLeft,
  rotateRight,
  doubleRotateRightLeft,
  doubleRotateLeftRight,
  insert
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
insert x (AVLNode v lf rt)
  | x < v =
    if ((height newlf - height rt) == 2)
       then if x < newlfv
          then rotateLeft (AVLNode v newlf rt)
          else doubleRotateLeftRight (AVLNode v newlf rt)
       else (AVLNode v newlf rt)
  | otherwise =
    if ((height newrt - height lf) == 2)
       then if x > newrtv
          then rotateRight (AVLNode v lf newrt)
          else doubleRotateRightLeft (AVLNode v lf newrt)
       else (AVLNode v lf newrt)
  where
      newlf@(AVLNode newlfv _ _) = insert x lf
      newrt@(AVLNode newrtv _ _) = insert x rt
