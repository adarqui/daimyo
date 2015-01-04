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

    wrecked. edit: working a little better.
-}

simplify'radical r k =
    let
        facs = rootFactors k
        (coeff', radicand') = findRoot (fromIntegral r) (map fromIntegral facs)
    in
        case (coeff') of
            Nothing -> NthRoot { root = r, coeff = 1, radicand = radicand' }
            otherwise -> NthRoot { root = r, coeff = fromJust $ coeff', radicand = radicand' }
        
simplifyRadical = simplify'radical


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


t_findRoot'1 = findRoot 2 [2,2,2,2,5]
t_simplify'radical'80 = simplify'radical 80
t_simplify'radical'25 = simplify'radical 25
t_simplify'radical'26 = simplify'radical 26
