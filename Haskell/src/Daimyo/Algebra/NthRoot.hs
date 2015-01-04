{-# LANGUAGE NPlusKPatterns #-}

module Daimyo.Algebra.NthRoot (
    nthRoot,
    rootFactors,
    simplify'radical,
    simplifyRadical,
    findRoot
) where

import Daimyo.Number
import Daimyo.NumberTheory.Prime
import Data.Maybe

{-
    1 * (NthRoot 2 80) = sqrt 80
    coeff = 1
    root = 2
    radicand = 80
-}

data NthRoot = NthRoot {
    coeff :: Double,
    root :: Integer,
    radicand :: Maybe Double
} deriving (Show, Read, Eq, Ord)

{-
    the nth root of k is: k^(1/r)
-}

nthRoot :: Floating a => a -> a -> a
nthRoot r k = k ** (1/r)


rootFactors n = primeFactors n


{-
    simplify an nth root

    sqrt (80) = sqrt (2*2*2*2*5) = sqrt (2*2*2*2) * sqrt (5) = 4 * sqrt 5

    wrecked.
-}

simplify'radical r k =
    let
        facs = rootFactors k
        (coeff', radicand') = findRoot (fromIntegral r) (map fromIntegral facs)
    in
        NthRoot {
            root = r,
            coeff = fromJust $ coeff',
            radicand = Just radicand'
        }
        
simplifyRadical = simplify'radical


findRoot r l = findRoot' r (reverse l) []

findRoot' r [] accum = (Nothing, product accum)
findRoot' r all@(k:ks) accum
    | isInteger $ nth = (Just nth, product accum)
    | otherwise = findRoot' r ks (k : accum)
    where
        nth = nthRoot r (product all)


t_findRoot'1 = findRoot 2 [2,2,2,2,5]
t_simplify'radical'80 = simplify'radical 80
