module Daimyo.Random.LCG.BSD (
  lcgBSD,
  lcgsBSD
) where

import           Daimyo.Control.Monad
import           Daimyo.Control.State
import           Daimyo.Random.LCG

-- | lcgBSD
--
lcgBSD :: State Int Int
lcgBSD = lcg (LCG 1103515245 12345 (2^31))

-- | lcgsBSD
--
-- >>> take 5 $ lcgsBSD 5
-- [1222621274,554244747,695785320,2089129857,668008486]
--
lcgsBSD :: Int -> [Int]
lcgsBSD seed = evalState (repeatM lcgBSD) seed
