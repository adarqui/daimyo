{-# LANGUAGE FlexibleInstances #-}

module Daimyo.Algebra.Fraction (
  Fraction,
  fraction,
  simplifyString,
  showFractionString,
  simplifyInteger
) where

import           Daimyo.NumberTheory.GCD

data Fraction a = Fraction {
  numerator   :: a,
  denominator :: a
} deriving (Show, Eq)

instance Num (Fraction String) where
  fa + fb
     | d fa == d fb = fraction (n fa ++ " + " ++ n fb) (d fa)
     | otherwise = fraction (n fa ++ d fb ++ " + " ++ n fb ++ d fa) (d fa ++ d fb)
  fa - fb
     | d fa == d fb = fraction (n fa ++ n fb) (d fa)
     | otherwise = fraction (n fa ++ d fb ++ " - " ++ n fb ++ d fa) (d fa ++ d fb)
  fa * fb = fraction (n fa ++ n fb) (d fa ++ d fb)

instance Num (Fraction Integer) where
  fa + fb
     | d fa == d fb = fraction (n fa + n fb) (d fa)
     | otherwise = fraction ((n fa * d fb) + (n fb * d fa)) (d fa * d fb)
  fa - fb
     | d fa == d fb = fraction (n fa - n fb) (d fa)
     | otherwise = fraction ((n fa * d fb) - (n fb * d fa)) (d fa * d fb)
  fa * fb = fraction (n fa * n fb) (d fa * d fb)
  abs fa = fraction (abs (n fa)) (abs (d fa))
  negate fa = fraction ((n fa) * (-1)) ((d fa) * (-1))

instance Enum (Fraction Integer) where
instance Ord (Fraction Integer) where
instance Real (Fraction Integer) where

instance Integral (Fraction Integer) where
  fa `div` fb = fa * (fraction (d fb) (n fb))

d = denominator
n = numerator

fraction :: a -> a -> Fraction a
fraction n d = Fraction n d

-- | simplifyString
--
-- for multiplication.. not sure what to do yet with addition/subtraction etc.. will need actual data types for that or a parser
--
simplifyString :: Eq a => Fraction [a] -> Fraction [a]
simplifyString f =
  fraction
    (filter (\c -> notElem c (d f)) (n f))
    (filter (\c -> notElem c (n f)) (d f))

-- | showFractionString
--
showFractionString :: Fraction String -> String
showFractionString f =
  "(("++(n f)++")/("++(d f)++"))"

-- | simplifyInteger
--
simplifyInteger :: Integral a => Fraction a -> Fraction a
simplifyInteger f = fraction ((n f) `div` gcdOf) ((d f) `div` gcdOf)
  where
    gcdOf = gcd (n f) (d f)

-- | Examples
--
-- >>> fraction "a" "b" + fraction "c" "d"
-- Fraction {numerator = "ad + cb", denominator = "bd"}
--
-- >>> (fraction "a" "b" + fraction "c" "d") + fraction "x" "y"
-- Fraction {numerator = "ad + cby + xbd", denominator = "bdy"}
--
-- >>> fraction "a" "" + fraction "b" "c"
-- Fraction {numerator = "ac + b", denominator = "c"}
--
-- >>> fraction "a" "b" + fraction "c" "b"
-- Fraction {numerator = "a + c", denominator = "b"}
--
-- >>> (fraction 3 4) + (fraction 5 4) :: Fraction Integer
-- Fraction {numerator = 8, denominator = 4}
--
-- >>> (fraction 2 3) - (fraction 5 3) :: Fraction Integer
-- Fraction {numerator = -3, denominator = 3}
--
-- >>> fraction 5 (-3) * fraction 7 11 :: Fraction Integer
-- Fraction {numerator = 35, denominator = -33}
--
-- >>> fraction 3 10 `div` fraction 4 7 :: Fraction Integer
-- Fraction {numerator = 21, denominator = 40}
--
-- >>> abs $ fraction (-1) 2 :: Fraction Integer
-- Fraction {numerator = 1, denominator = 2}
