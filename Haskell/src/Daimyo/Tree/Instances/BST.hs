module Daimyo.Tree.Instances.BST (
 module Data.Monoid,
 module Data.Foldable
) where

import Data.Monoid
import Data.Functor
import Data.Foldable
import Data.Traversable
import Control.Applicative

data BST k v = Empty | Node k v (BST k v) (BST k v) deriving (Eq, Show)

instance Monoid (BST k v) where
 mempty = Empty
 mappend Empty n = n
-- mappend (Node a l r) n@(Node a' l' r') = if a < a' then (mappend l

instance Foldable (BST k) where
 foldMap _ Empty = mempty
 foldMap f (Node k v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

instance Functor (BST k) where
 fmap f Empty = Empty
 fmap f (Node k v l r) = Node k (f v) (fmap f l) (fmap f r)

{-
instance Applicative BST where
 pure x = Node x Empty Empty
 Empty <*> _ = Empty
-- f <*> (Node a l r) = (Node (f a) l r)
 f <*> (Node a l r) = Empty
-}

fromList :: (Ord k) => [(k,v)] -> BST k v
fromList [] = Empty
fromList ((k,v):xs) = Node k v (fromList $ takeWhile (\(x,y) -> x < k) xs) (fromList $ takeWhile (\(x,y) -> x > k) xs)

-- simple examples

t1 = fmap (+1) $ fromList $ zip [1..5] [1..5]
t2 = 100 <$ fromList $ zip [1..5] [1..5]
t2b = fromList (zip [1..5] [1..5]) $> 100
--t3 = (*2) <$> fromList $ zip [10..20] [10..20]
--t4 = (+1) <$> (Node 1 Empty Empty)
