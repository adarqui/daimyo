{-# LANGUAGE NPlusKPatterns #-}

module Daimyo.Algebra.NthRoot (
  nthRoot,
  rootFactors,
  simplifyRadical,
  simplifyRadical,
  findRoot
) where

import           Daimyo.Number
import           Daimyo.NumberTheory.Prime
import           Data.Maybe

-- | NthRoot
--
-- 1 * (NthRoot 2 80) = sqrt 80
-- coeff = 1
-- root = 2
-- radicand = 80
--
data NthRoot = NthRoot {
    coeff    :: Double,
    root     :: Integer,
    radicand :: Maybe Double
} deriving (Show, Read, Eq, Ord)

-- | nthRoot
--
-- the nth root of k is: k^(1/r)
--
-- >>> nthRoot 2 25 :: Double
-- 5.0
--
nthRoot :: Floating a => a -> a -> a
nthRoot r k = k ** (1/r)

-- | rootFactors
--
-- >>> rootFactors 125
-- [5,5,5]
--
rootFactors :: Integer -> [Integer]
rootFactors = primeFactors

-- | simplifyRadical
--
--  simplify an nth root
--  sqrt (80) = sqrt (2*2*2*2*5) = sqrt (2*2*2*2) * sqrt (5) = 4 * sqrt 5
--  wrecked. edit: working a little better.
--
-- >>> simplifyRadical 2 25
-- NthRoot {coeff = 5.0, root = 2, radicand = Nothing}
--
-- >>> simplifyRadical 3 125
-- NthRoot {coeff = 1.0, root = 3, radicand = Just 125.0}
--
simplifyRadical :: Integer -> Integer -> NthRoot
simplifyRadical r k = NthRoot { root=r, coeff=maybe 1 id coeff', radicand=radicand' }
  where
    (coeff', radicand') = findRoot (fromIntegral r) (map fromIntegral $ rootFactors k)

-- | findRoot
--
-- eek
--
-- >>> findRoot (2 :: Double) [2,2,2,2,5]
-- (Just 4.0,Just 5.0)
--
findRoot r l = findRoot' r (reverse l) []
findRoot' r [] accum = (Nothing, accum')
  where
    accum' = if (accum == []) then Nothing else (Just $ product accum)
findRoot' r all@(k:ks) accum
  | isInteger $ nth = (Just nth, accum')
  | otherwise = findRoot' r ks (k : accum)
  where
    nth = nthRoot r (product all)
    accum' = if (accum == []) then Nothing else (Just $ product accum)
