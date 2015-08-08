module Daimyo.Algorithm.Network.ProcessNetwork (
  Channel,
  Process,
  ProcessList,
  ConnectionTable,
  processNetwork
) where

import           Daimyo.Table.Array
import           GHC.Arr

type Channel a       = [a]
type Process a       = [Channel a] -> [Channel a]
type ProcessList a   = [Process a]
type ConnectionTable = Table [(Int,Int)] Int

-- | processNetwork
--
-- mostly taken from AAFA
--
processNetwork :: ProcessList a -> ConnectionTable -> Channel a -> Channel a
processNetwork ps ct input = (outputs!n) !! 0
  where
    outputs = array (0,n) ([(0, [input])] ++ [ (m, ((ps!!(m-1))(parameters m))) | m <- [1..n]] )
    parameters i = [ (outputs!p)!!o | (p,o) <- findTable i ct]
    n = length ps
