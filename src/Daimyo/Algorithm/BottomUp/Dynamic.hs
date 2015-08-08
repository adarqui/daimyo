module Daimyo.Algorithm.BottomUp.Dynamic (
  Compute,
  Bounds,
  dynamic
) where

import           Daimyo.Table.Array
import           GHC.Arr

type Compute coord entry = Table entry coord -> coord -> entry
type Bounds coord        = (coord, coord)

-- | dynamic
--
-- mostly taken from AAFA
--
dynamic :: Ix coord => (Table entry coord -> coord -> entry) -> (coord, coord) -> Table entry coord
dynamic compute bounds' = t
  where
    t = newTable (map (\coord -> (coord, compute t coord)) (range bounds'))
