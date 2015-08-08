module Daimyo.Algorithm.Examples.Hanoi (
  hanoi
) where

-- | hanoi
--
-- >>> hanoi 4 1 3 :: [(Int,Int,Int)]
-- [(1,1,2),(2,1,3),(1,2,3),(3,1,2),(1,3,1),(2,3,2),(1,1,2),(4,1,3),(1,2,3),(2,2,1),(1,3,1),(3,2,3),(1,1,2),(2,1,3),(1,2,3)]
--
hanoi :: (Num a, Num t, Eq t) => t -> a -> a -> [(t, a, a)]
hanoi 1 fromPeg toPeg = [(1, fromPeg, toPeg)]
hanoi n fromPeg toPeg = a ++ [(n, fromPeg, toPeg)] ++ b
  where
    unusedPeg = 6 - fromPeg - toPeg
    a         = hanoi (n-1) fromPeg unusedPeg
    b         = hanoi (n-1) unusedPeg toPeg
