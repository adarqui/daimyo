module Daimyo.NumberTheory.ContinuedFraction (
    continuedFraction,
    continuedFraction'rest,
    continuedFractionNumerators,
    continuedFractionNumerators'rest
) where

continuedFraction (n,dl) = n + continuedFraction'rest dl

continuedFraction'rest ds = 1 / continuedFraction'rest' ds

continuedFraction'rest' [] = 0
continuedFraction'rest' (d:ds) = d + (1 / continuedFraction'rest' ds)

t_continuedFraction'1 = continuedFraction (4,[2,6,7])


{-
    allow numerators to be specified
-}

continuedFractionNumerators (n,nl,dl) = n + continuedFractionNumerators'rest nl dl

continuedFractionNumerators'rest (n:ns) ds = n / continuedFractionNumerators'rest' ns ds

continuedFractionNumerators'rest' _ [] = 0
continuedFractionNumerators'rest' [] _ = 0
continuedFractionNumerators'rest' (n:ns) (d:ds) = d + (n / continuedFractionNumerators'rest' ns ds)

t_pi = continuedFractionNumerators (3, [1,6,6,6,6,6], [1,9,25,49,81,121])
