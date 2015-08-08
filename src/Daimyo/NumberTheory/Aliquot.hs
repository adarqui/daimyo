module Daimyo.NumberTheory.Aliquot (
  aliquotParts,
  amicablePair,
  aliquotSequence,
  aliquotPerfect
) where

import           Daimyo.Algebra.Divisibility

-- | aliquotParts
--
-- >>> aliquotParts 28
-- [1,2,4,7,14]
--
aliquotParts :: Integer -> [Integer]
aliquotParts = properDivisors

-- | amicablePair
--
amicablePair :: t
amicablePair = undefined

-- | aliquotSequence
--
-- >>> aliquotSequence 24
-- [24,36,55,17,1]
--
-- >>> take 10 $ aliquotSequence 28
-- [28,28,28,28,28,28,28,28,28,28]
--
aliquotSequence :: Integer -> [Integer]
aliquotSequence n
  | n == 1 = [1]
  | otherwise = n : aliquotSequence (sum $ aliquotParts n)

-- | aliquotPerfect
--
-- >>> aliquotPerfect 28
-- True
--
-- >>> aliquotPerfect 27
-- False
--
aliquotPerfect :: Integer -> Bool
aliquotPerfect n
  | len == 2  = True
  | otherwise = False
  where
    as  = aliquotSequence n
    len = length $ take 2 $ takeWhile (==n) as
