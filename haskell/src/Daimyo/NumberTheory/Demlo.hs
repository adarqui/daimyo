{-# LANGUAGE ParallelListComp #-}

module Daimyo.NumberTheory.Demlo (
  demlo,
  demlos,
  demloOnes,
  demloOne
) where

import Daimyo.Number
import Data.List

-- | demlos
--
-- Demlo numbers: really cool.
--
-- >>> take 15 demlos
-- [121,12321,1234321,123454321,12345654321,1234567654321,123456787654321,12345678987654321,1234567900987654321,123456790120987654321,12345679012320987654321,1234567901234320987654321,123456790123454320987654321,12345679012345654320987654321,1234567901234567654320987654321]
--
demlos :: [Integer]
demlos = tail [ x*x | x <- demloOnes ]

-- | demloOnes
--
-- >>> take 10 demloOnes
-- [1,11,111,1111,11111,111111,1111111,11111111,111111111,1111111111]
--
demloOnes :: [Integer]
demloOnes = scanl (+) 1 by10

-- | demlo
--
-- >>> demlo 9
-- 12345678987654321
--
demlo :: Int -> Integer
demlo n = d*d
  where d = demloOne n

-- | demloOne
--
-- >>> demloOne 9
-- 111111111
--
demloOne :: Int -> Integer
demloOne n = foldl' (+) 1 $ take (n-1) by10
