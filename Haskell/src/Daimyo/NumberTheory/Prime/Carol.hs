module Daimyo.NumberTheory.Prime.Carol (
    carol
) where

carol = [ (2^n - 1)^2 - 2 | n <- [2..] ]
