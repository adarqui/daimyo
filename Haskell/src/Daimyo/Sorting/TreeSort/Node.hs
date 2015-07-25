module Daimyo.Sorting.TreeSort.Node (
  tsort
) where

import           Daimyo.Tree.Node

-- | tsort
--
-- >>> tsort [1,3,1,9,4,2,3,0,4,5,10,0,0,7,6] :: [Int]
-- [0,1,2,3,4,5,6,7,9,10]
--
-- eek
--
tsort :: Ord a => [a] -> [a]
tsort = inOrder . fromList
