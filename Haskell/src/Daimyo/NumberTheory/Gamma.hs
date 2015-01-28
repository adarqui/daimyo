module Daimyo.NumberTheory.Gamma (
) where

import Math.Gamma

{-
    some tests:

    http://www.wolframalpha.com/input/?i=%28log2%285%29%29%21
-}

t_gamma :: [Double]
t_gamma = [gamma (1 + (logBase 2 5)), gamma (1 + (logBase 2 10))]
