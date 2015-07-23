module Daimyo.Set.Naive (
  Set,
  emptySet,
  isEmptySet,
  containedInSet,
  addSet,
  delSet
) where

data Set a
  = Set a
  deriving (Eq, Ord, Show)

emptySet = undefined
isEmptySet = undefined
containedInSet = undefined
addSet = undefined
delSet = undefined
