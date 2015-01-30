module Daimyo.NumberTheory.ContinuedFraction (
    continuedFraction,
    continuedFraction'list
) where

continuedFraction (n,dl) = n + continuedFraction'list dl

continuedFraction'list ds = 1 / continuedFraction'list' ds

continuedFraction'list' [] = 0
continuedFraction'list' (d:ds) = d + (1 / continuedFraction'list' ds)

t_continuedFraction'1 = continuedFraction (4,[2,6,7])
