module Daimyo.Tree.Instances.Node (
 module Data.Monoid,
 module Data.Foldable
) where

import Data.Monoid
import Data.Functor
import Data.Foldable
import Data.Traversable
import Control.Applicative

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

instance Monoid (Tree a) where
 mempty = Empty
 mappend Empty n = n
-- mappend (Node a l r) n@(Node a' l' r') = if a < a' then (mappend l

instance Foldable Tree where
 foldMap _ Empty = mempty
 foldMap f (Node a l r) = foldMap f l `mappend` f a `mappend` foldMap f r

instance Functor Tree where
 fmap f Empty = Empty
 fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative Tree where
 pure x = Node x Empty Empty
 Empty <*> _ = Empty
-- f <*> (Node a l r) = (Node (f a) l r)
 f <*> (Node a l r) = Empty

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = Node x (fromList $ takeWhile (< x) xs) (fromList $ takeWhile (> x) xs)

-- simple examples

t1 = fmap (+1) $ fromList [1..5]
t2 = 100 <$ fromList [1..5]
t2b = fromList [1..5] $> 100
t3 = (*2) <$> fromList [10..20]
t4 = (+1) <$> (Node 1 Empty Empty)
