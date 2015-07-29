module Daimyo.NumberTheory.Nines (
  nines
) where

import           Daimyo.Number

-- | nines
--
-- >>> nines 999999999999999 :: Int
-- 0
--
nines :: Integral a => a -> a
nines n
  | n < 10 = if (n == 9) then 0 else n
  | otherwise = nines $ sum $ map (\x -> if (x == 9) then 0 else x) (digits n)
