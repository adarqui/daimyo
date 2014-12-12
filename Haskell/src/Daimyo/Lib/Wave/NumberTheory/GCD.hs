{-
    sources:
        GCD
            http://en.wikipedia.org/wiki/Greatest_common_divisor
-}

module Daimyo.Lib.Wave.NumberTheory.GCD (
    gcd
) where

{-
    GCD
-}

gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

gcf = gcd'
hcf = gcd'
gcm = gcd'
hcd = gcd'

gcd'' a b
    | a == b = a
    | a > b = gcd (a-b) b
    | b > a = gcd a (b-a)
