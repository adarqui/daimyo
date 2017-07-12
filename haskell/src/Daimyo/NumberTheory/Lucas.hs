module Daimyo.NumberTheory.Lucas (
    lucas,
    lucasNumbers
) where

lucasNumbers = 2 : 1 : lucasNumbers' 1 3
lucasNumbers' p c = (p + c) : lucasNumbers' c (p+c)

lucas 0 = 2
lucas 1 = 1
lucas n = lucas (n-1) + lucas (n-2)
