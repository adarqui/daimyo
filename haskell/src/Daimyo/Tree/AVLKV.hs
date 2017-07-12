{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Daimyo.Tree.AVLKV (
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
data AVLTree k v
  = AVLEmpty
  | AVLNode k v (AVLTree k v) (AVLTree k v)
  deriving (Eq, Ord, Show)

-- | empty
--
empty :: AVLTree k v
empty = AVLEmpty

-- | height
--
height :: AVLTree k v -> Int
height AVLEmpty = 0
height (AVLNode _ _ lf rt) = 1 + max (height lf) (height rt)

-- | rotateLeft
--
rotateLeft :: AVLTree k v -> AVLTree k v
rotateLeft AVLEmpty = AVLEmpty
rotateLeft (AVLNode k v (AVLNode lk lv lflf lfrt) rt)
  = AVLNode lk lv lflf (AVLNode k v lfrt rt)

-- | rotateRight
--
rotateRight :: AVLTree k v -> AVLTree k v
rotateRight AVLEmpty = AVLEmpty
rotateRight (AVLNode k v lf (AVLNode rk rv rtlf rtrt))
  = AVLNode rk rv (AVLNode k v lf rtlf) rtrt

-- | doubleRotateLeft
--
doubleRotateRightLeft :: AVLTree k v -> AVLTree k v
doubleRotateRightLeft AVLEmpty = AVLEmpty
doubleRotateRightLeft (AVLNode k v lf (AVLNode rk rv (AVLNode rtlk rtlv rtlflf rtlfrt) rtrt))
  = AVLNode rtlk rtlv (AVLNode k v lf rtlflf) (AVLNode rk rv rtlfrt rtrt)

-- | doubleRotateRight
--
doubleRotateLeftRight :: AVLTree k v -> AVLTree k v
doubleRotateLeftRight AVLEmpty = AVLEmpty
doubleRotateLeftRight (AVLNode k v (AVLNode lk lv lflf (AVLNode lfrk lfrv lfrtlf lfrtrt)) rt)
  = AVLNode lfrk lfrv (AVLNode lk lv lflf lfrtlf) (AVLNode k v lfrtrt rt)

-- | insert
--
insert :: Ord k => k -> v -> AVLTree k v -> AVLTree k v
insert k v AVLEmpty = AVLNode k v AVLEmpty AVLEmpty
insert k v t@(AVLNode k' _ lf rt)
  | k < k' =
    if ((height newlf - height rt) == 2)
       then if k < newlfk
          then rotateLeft (AVLNode k v newlf rt)
          else doubleRotateLeftRight (AVLNode k v newlf rt)
       else (AVLNode k v newlf rt)
  | k == k' = t
  | otherwise =
    if ((height newrt - height lf) == 2)
       then if k > newrtk
          then rotateRight (AVLNode k v lf newrt)
          else doubleRotateRightLeft (AVLNode k v lf newrt)
       else (AVLNode k v lf newrt)
  where
      newlf@(AVLNode newlfk _ _ _) = insert k v lf
      newrt@(AVLNode newrtk _ _ _) = insert k v rt

-- | fromList
--
-- >>> fromList [(1,2),(2,1),(1,3)] :: AVLTree Int Int
-- AVLNode 1 2 (AVLNode 1 2 AVLEmpty AVLEmpty) (AVLNode 2 1 AVLEmpty AVLEmpty)
--
fromList :: Ord k => [(k, v)] -> AVLTree k v
fromList pairs = foldr f AVLEmpty pairs
  where
    f (k, v) acc = insert k v acc

-- | toList
--
-- >>> fromList [(9,"nine"),(0,"oh"),(2,"two"),(1,"one"),(0,"oh")] :: AVLTree Int String
-- breaks it.. need to fix
--
toList :: AVLTree k v -> [(k, v)]
toList = inOrder

-- | preOrder
--
-- >>> preOrder (fromList [9,0,2,1,0,4,8] :: AVLTree Int)
-- [4,1,0,2,8,9]
--
preOrder :: AVLTree k v -> [(k, v)]
preOrder AVLEmpty = []
preOrder (AVLNode k v l r) = (k, v) : preOrder l ++ preOrder r

-- | inOrder
--
-- >>> inOrder (fromList [9,0,2,1,0,4,8] :: AVLTree Int)
-- [0,1,2,4,8,9]
--
inOrder :: AVLTree k v -> [(k, v)]
inOrder AVLEmpty = []
inOrder (AVLNode k v l r) = inOrder l ++ [(k, v)] ++ inOrder r

-- | postOrder
--
postOrder :: AVLTree k v -> [(k, v)]
postOrder AVLEmpty = []
postOrder (AVLNode k v l r) = postOrder l ++ postOrder r ++ [(k, v)]
