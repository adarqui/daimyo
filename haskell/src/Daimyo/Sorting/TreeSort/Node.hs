module Daimyo.Sorting.TreeSort.Node (
  tsort
) where

import           Daimyo.Tree.Node.Dups

-- | tsort
--
-- >>> tsort [1,3,1,9,4,2,3,0,4,5,10,0,0,7,6] :: [Int]
-- [0,0,0,1,1,2,3,3,4,4,5,6,7,9,10]
--
tsort :: Ord a => [a] -> [a]
tsort = inOrder . fromList
