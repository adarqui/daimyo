module Daimyo.Sorting.SelectionSort.Naive (
  ssort,
  ssortBy
) where

import           Data.List

-- | ssort
--
-- >>> ssort [1,7,4,3,0,9] :: [Int]
-- [0,1,3,4,7,9]
--
ssort :: Ord a => [a] -> [a]
ssort = ssortBy minimum

-- | ssortBy
--
-- >>> ssortBy maximum [1,7,4,3,0,9] :: [Int]
-- [9,7,4,3,1,0]
--
ssortBy :: Ord a => ([a] -> a) -> [a] -> [a]
ssortBy _ []  = []
ssortBy by xs = m : ssortBy by (delete m xs)
  where
    m = by xs
