module Daimyo.NumberTheory.ContinuedFraction (
    continuedFraction,
    continuedFraction'rest
) where

continuedFraction (n,dl) = n + continuedFraction'rest dl

continuedFraction'rest ds = 1 / continuedFraction'rest' ds

continuedFraction'rest' [] = 0
continuedFraction'rest' (d:ds) = d + (1 / continuedFraction'rest' ds)

t_continuedFraction'1 = continuedFraction (4,[2,6,7])
