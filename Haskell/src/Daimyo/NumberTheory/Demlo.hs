{-# LANGUAGE ParallelListComp #-}

module Daimyo.NumberTheory.Demlo (
    demlo,
    demlos,
    demlo'ones,
    demlo'one
) where

import Daimyo.Number
import Data.List

{-
    really cool.

    take 15 demlos 

[121,12321,1234321,123454321,12345654321,1234567654321,123456787654321,12345678987654321,1234567900987654321,123456790120987654321,12345679012320987654321,1234567901234320987654321,123456790123454320987654321,12345679012345654320987654321,1234567901234567654320987654321]
-}

demlos = tail [ x*x | x <- demlo'ones ]


demlo'ones = scanl (\acc x -> acc + x) 1 by10


demlo n =
    let
        d = demlo'one n
    in
        d*d


demlo'one n = foldl' (\acc x -> acc + x) 1 $ take (n-1) by10


t_demlo = demlo 9 -- 12345678987654321
