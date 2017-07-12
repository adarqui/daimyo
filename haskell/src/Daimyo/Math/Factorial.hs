module Daimyo.Math.Factorial (
    fac,
    fac'product,
    fac'hist,
    fac'list
) where

fac 0 = 1
fac n = n * fac (n-1)

fac'product n = product [1..n]

fac'hist 0 = (1, [1])
fac'hist n =
    let
        (r,l) = fac'hist (n-1)
    in
        (r*n, l ++ [r*n])

fac'list n = let (_, l) = fac'hist n in l
