module Daimyo.Algebra.Exponent (
    negPow
) where

{-
    for my bernoulli functions
-}

negPow x n
    | x < 0 =
        if (odd (floor n))
            then (-1)*((abs x)**n)
            else ((abs x)**n)
    | x >= 0 = x ** n
