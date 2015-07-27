module Daimyo.Algorithm.Examples.Coins (
  change
) where

import           Daimyo.Algorithm.TopDown.GreedySearch

type SolutionChange = [Int]
type NodeChange     = (Int, SolutionChange)

-- | coins
--
coins :: [Int]
coins = [1,5,10,25,50,100]

-- | succCoins
--
succCoins :: NodeChange -> [NodeChange]
succCoins (r,p) = [ (r-c,c:p) | c <- coins, r-c >= 0 ]

-- | goalCoins
--
goalCoins :: NodeChange -> Bool
goalCoins (v,_) = v == 0

-- | change
--
-- >>> change 1050
-- [50,100,100,100,100,100,100,100,100,100,100]
--
-- >>> change 128
-- [1,1,1,25,100]
--
change :: Int -> SolutionChange
change amount = snd $ head $ searchGreedy succCoins goalCoins (amount, [])
