module Daimyo.Algebra.Permutation.Cycle (
 kcycle,
  cyclek,
  cycle2,
  cycle3
) where

import           Data.List

-- | kcycle
--
-- >>> kcycle 2 ([1,2,3,4] :: [Int])
-- [[1,2,3,4],[2,1,3,4]]
--
kcycle :: Int -> [a] -> [[a]]
kcycle k s = map (++ b) $ permutations a
  where
    a = take k s
    b = drop k s

-- | cyclek
--
cyclek :: Int -> [a] -> [[a]]
cyclek = kcycle

-- | cycle2
--
cycle2 :: [a] -> [[a]]
cycle2 = kcycle 2

-- | cycle3
--
cycle3 :: [a] -> [[a]]
cycle3 = kcycle 3
