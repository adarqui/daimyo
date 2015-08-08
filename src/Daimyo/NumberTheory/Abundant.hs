module Daimyo.NumberTheory.Abundant (
    abundants,
    abundant,
    deficients,
    perfects,
    isAbundant,
    isDeficient,
    isPerfect
) where

import Daimyo.NumberTheory.Prime
import Data.List

data Abundant = Deficient | Perfect | Abundant deriving (Show, Read, Eq)

abundants = [ n | n <- [2..], isAbundant n ]

abundant n =
    let
        r = sum $ 1 : (filter (/= n) $ primeMultiples'products n)
    in
        case (compare r n) of
            GT -> Abundant
            EQ -> Perfect
            LT -> Deficient


isAbundant n = abundant n == Abundant
isDeficient n = abundant n == Deficient
isPerfect n = abundant n == Perfect


deficients = 1 : [ n | n <- [3,5..], isDeficient n ]


perfects = [ n | n <- [2..], isPerfect n ]


t_abundant'16 = abundant 16
