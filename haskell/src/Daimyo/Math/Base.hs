module Daimyo.Math.Base (
  factorBase,
  translateFactorBase
) where

import Daimyo.Number

-- | factorBase
--
-- >>> factorBase 2 12 :: [Int]
-- [1,1,0,0]
--
-- >>> factorBase 2 64 :: [Int]
-- [1,0,0,0,0,0,0]
--
-- >>> factorBase 16 42 :: [Int]
-- [2,10]
--
factorBase :: Integral a => a -> a -> [a]
factorBase _ 0 = []
factorBase b n = (factorBase b q) ++ [r]
  where (q,r) = quotRem n b

-- | translateFactorBas
--
translateFactorBase :: Integer -> Integer -> Integer
translateFactorBase b n = sum $ map (\(b',p) -> b'*p) r'facs
  where
  facs   = factorBase b n
  r'facs = zip (reverse facs) oneBy10
