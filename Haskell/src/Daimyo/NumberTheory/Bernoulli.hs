module Daimyo.NumberTheory.Bernoulli (
    bernoullis,
    bernoulli
) where

import Daimyo.NumberTheory.Factorial
import Daimyo.NumberTheory.Constants.E

bernoullis = [ bernoulli x | x <- [1..] ]

bernoulli 0 = 1
bernoulli x = x / ((e**x) - 1)

{-
    implement this: http://wstein.org/projects/168/kevin_mcgown/bernproj.pdf
-}

k m = (2*fac m)/((2*pi)**m)

--d = product p


