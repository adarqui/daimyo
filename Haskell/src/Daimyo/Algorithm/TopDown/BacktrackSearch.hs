module Daimyo.Algorithm.TopDown.BacktrackSearch (
  searchDFS
) where

import qualified Daimyo.Stack.List as S

type Successor n = n -> [n]
type Goal n      = n -> Bool
type Initial n   = n
type Solution n  = [n]

-- | searchDFS
--
searchDFS :: Eq node => Successor node -> Goal node -> Initial node -> Solution node
searchDFS successor goal initial = go (S.push initial S.empty)
  where
    go stk
      | S.isEmpty stk     = []
      | goal (S.top' stk) = S.top' stk : go (S.pop' stk)
      | otherwise         = go (foldr S.push (S.pop' stk) (successor (S.top' stk)))
