module Daimyo.Random.LCG.MS (
  lcgMS,
  lcgsMS
) where

import           Daimyo.Control.Monad
import           Daimyo.Control.State
import           Daimyo.Random.LCG

-- | lcgMS
--
lcgMS :: State Int Int
lcgMS = lcg (LCG 214013 2531011 (2^(31::Int)))

-- | lcgsMS
--
-- >>> take 5 $ lcgsMS 5
-- [3601076,1880463015,803157710,1602335321,1812736952]
--
lcgsMS :: Int -> [Int]
lcgsMS seed = evalState (repeatM lcgMS) seed
