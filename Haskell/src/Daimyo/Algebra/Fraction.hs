{-# LANGUAGE FlexibleInstances #-}

module Daimyo.Algebra.Fraction (
    Fraction,
    fraction
) where

import Daimyo.NumberTheory.GCD

data Fraction a = Fraction {
    numerator :: a,
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

fraction n d = Fraction n d

-- for multiplication.. not sure what to do yet with addition/subtraction etc.. will need actual data types for that or a parser
simplify'string f =
    fraction
        (filter (\c -> notElem c (d f)) (n f))
        (filter (\c -> notElem c (n f)) (d f))

show'string f =
    "(("++(n f)++")/("++(d f)++"))"

simplify'integer f =
    let gcdOf = gcd (n f) (d f) in
    fraction ((n f) `div` gcdOf) ((d f) `div` gcdOf)

t_frac_str_add1 = fraction "a" "b" + fraction "c" "d"
t_frac_str_add2 = t_frac_str_add1 + fraction "x" "y"

t_frac_str_add3 = fraction "a" "" + fraction "b" "c"
t_frac_str_add4 = fraction "a" "b" + fraction "c" "b"

t_frac_str_mul1 = fraction "a" "b" * fraction "c" "d"
t_frac_str_mul2 = fraction "a" "" * fraction "b" "c"
t_frac_str_mul3 = fraction "a" "b" * fraction "c" ""
t_frac_str_mul4 = t_frac_str_mul3 * fraction "z" "b"
t_frac_str_mul5 = t_frac_str_mul3 * fraction "z" "c"
t_frac_str_mul5' = simplify'string t_frac_str_mul5
t_frac_str_mul6 = fraction "a" "" * fraction "b" "a"
t_frac_str_mul6' = simplify'string t_frac_str_mul6
t_frac_str_mul6'' = t_frac_str_mul6' * fraction "" "b"
t_frac_str_mul6''' = simplify'string t_frac_str_mul6''

{-
    addition
-}

t_frac_int_add1 :: Fraction Integer
t_frac_int_add1 = (fraction 3 4) + (fraction 5 4)

t_frac_int_add2 :: Fraction Integer
t_frac_int_add2 = (fraction 6 1) + (fraction 8 5)

t_frac_int_add3 :: Fraction Integer
t_frac_int_add3 = (fraction 6 7) + (fraction 3 4)

t_frac_int_add = t_frac_int_add1 + t_frac_int_add2 + t_frac_int_add3

{-
    subtraction
-}

t_frac_int_sub1 :: Fraction Integer
t_frac_int_sub1 = (fraction 2 3) - (fraction 5 3)

t_frac_int_sub2 :: Fraction Integer
t_frac_int_sub2 = (fraction 1 1) - (fraction 9 4)

t_frac_int_sub3 :: Fraction Integer
t_frac_int_sub3 = (fraction 15 7) - (fraction 2 1)

t_frac_int_sub = t_frac_int_sub1 - t_frac_int_sub2 - t_frac_int_sub3

{-
    multiplication
-}

t_frac_int_mul1 :: Fraction Integer
t_frac_int_mul1 = fraction 5 (-3) * fraction 7 11

t_frac_int_mul2 :: Fraction Integer
t_frac_int_mul2 = fraction 6 1 * fraction 2 7

t_frac_int_mul3 :: Fraction Integer
t_frac_int_mul3 = fraction 9 5 * fraction (-3) 1

{-
    division
-}

t_frac_int_div1 :: Fraction Integer
t_frac_int_div1 = fraction 3 10 `div` fraction 4 7

t_frac_int_div2 :: Fraction Integer
t_frac_int_div2 = fraction 2 3`div` fraction 6 1
t_frac_int_div2' = simplify'integer t_frac_int_div2

t_frac_int_div3 :: Fraction Integer
t_frac_int_div3 = fraction 2 1 `div` fraction 7 4
t_frac_int_div3' = simplify'integer t_frac_int_div3

{-
    absolute value
-}
t_frac_int_abs1 :: Fraction Integer
t_frac_int_abs1 = abs $ fraction (-1) 2
