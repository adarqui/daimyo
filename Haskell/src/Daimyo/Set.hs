module Daimyo.Set (
 Set,
 fromList,
 toList,
 smembers,
-- union,
-- interaction,
-- difference
) where

import qualified Daimyo.Tree.Node as T

type Set v = T.Tree v

fromList :: Ord a => [a] -> Set a
fromList = T.fromList

smembers, toList :: Ord a => Set a -> [a]
toList = T.toList
smembers = toList

--union = T.union

--intersection = T.intersection

--difference = T.difference
