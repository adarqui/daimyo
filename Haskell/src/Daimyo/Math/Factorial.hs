module Daimyo.Math.Factorial (
    fac,
    fac'product
) where

fac 0 = 1
fac n = n * fac (n-1)

fac'product n = product [1..n]
