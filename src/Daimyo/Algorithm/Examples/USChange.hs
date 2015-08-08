module Daimyo.Algorithm.Examples.USChange (
  change,
  uschange,
  uscoins
) where

-- | change
--
change _ 0     = []
change coins m = c : change coins (m-c)
  where
    c = head $ filter (<=m) coins

-- | uscoins
--
uscoins :: [Integer]
uscoins = [25,10,5,1]

-- | uschange
--
-- >>> uschange 46
-- [25,10,10,1]
--
uschange :: Integer -> [Integer]
uschange = change uscoins
