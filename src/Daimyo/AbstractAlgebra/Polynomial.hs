{-# LANGUAGE NPlusKPatterns #-}

--  sources:
--  http://en.wikipedia.org/wiki/Binomial_theorem

module Daimyo.AbstractAlgebra.Polynomial (
  fac,
  binomial,
  pascalData,
) where

import           Daimyo.Combinatorics.Combination

-- | fac
--
-- >>> fac 5 :: Int
-- 120
--
fac :: Integral a => a -> a
fac 0     = 1
fac (n+1) = (n+1) * fac n

--    (x + y)^n = sum from k=0 to n of(n `choose` k)x^(n-k)y^k
--    (x + y)^n = sum from k=0 to n of(n `choose` k)x^ky^(n-k)

-- | binomial
--
-- >>> binomial 5 5 5 :: [Int]
-- [3125,15625,31250,31250,15625,3125]
--
binomial :: Integral a => a -> a -> a -> [a]
binomial x y n = map formula [0..n]
  where
    formula k = (n `choose` k) * (x^(n-k))*(y^k)

-- | pascalData
--
-- >>> pascalData 5 5 5 :: [[Int]]
-- [[5,5],[25,50,25],[125,375,375,125],[625,2500,3750,2500,625],[3125,15625,31250,31250,15625,3125]]
--
pascalData :: Integral a => a -> a -> a -> [[a]]
pascalData x y n = map (binomial x y) [1..n]
