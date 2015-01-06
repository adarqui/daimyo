module Daimyo.NumberTheory.Prime.Centered (
    decagonal,
    heptagonal,
    square,
    triangular
) where

import Daimyo.NumberTheory.Prime

decagonal = filter isPrime [ 5*(n^2 - n) + 1 | n <- [2..] ]
heptagonal = filter isPrime [ (7*n^2 - 7*n + 2) `div` 2 | n <- [2..] ]
square = filter isPrime [ n^2 + (n+1)^2 | n <- [1..] ]
triangular = filter isPrime [ (3*n^2 + 3*n + 2) `div` 2 | n <- [2..] ]
