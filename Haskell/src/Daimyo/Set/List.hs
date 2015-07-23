module Daimyo.Set.List (
  Set,
  emptySet,
  isEmptySet,
  containedInSet,
  addSet,
  delSet
) where

import           Data.List

newtype Set a
  = Set [a]
  deriving (Eq, Ord, Show)

-- | emptySet
--
emptySet :: Set a
emptySet = Set []

-- | isEmptySet
--
isEmptySet :: Set a -> Bool
isEmptySet (Set []) = True
isEmptySet _        = False

-- | containedInSet
--
containedInSet :: Eq a => a -> Set a -> Bool
containedInSet _ (Set []) = False
containedInSet x (Set xs) = elem x xs

-- | addSet
--
addSet :: Eq a => a -> Set a -> Set a
addSet x set@(Set xs)
  | x `containedInSet` set = set
  | otherwise              = Set (x:xs)

-- | delSet
--
delSet :: Eq a => a -> Set a -> Set a
delSet x (Set xs) = Set (delete x xs)
