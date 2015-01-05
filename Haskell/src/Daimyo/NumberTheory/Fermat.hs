module Daimyo.NumberTheory.Fermat (
    _F,
    fermat,
    fermats
) where

fermat n = 2^(2^n)+1
_F = fermat

fermats = map fermat [0..]
