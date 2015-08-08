module Daimyo.NumberTheory.Square (
    aristotle'square,
    square'palindrome
) where


{-
    n^2 = 1 + 2 + ... n + 2 + 1
-}

aristotle'square n = sum $ square'palindrome n


square'palindrome 1 = [1]
square'palindrome n =
    let
        factors = [1..n]
        rev'factors = tail $ reverse factors
        sequence = factors ++ rev'factors
    in
        sequence
