module Daimyo.Algorithm.Examples.Hanoi (
    hanoi
) where

hanoi 1 fromPeg toPeg = [(1, fromPeg, toPeg)] 
hanoi n fromPeg toPeg =
    let
        unusedPeg = 6 - fromPeg - toPeg
        a = hanoi (n-1) fromPeg unusedPeg
        b = hanoi (n-1) unusedPeg toPeg
    in
        a ++ [(n, fromPeg, toPeg)] ++ b

t_hani = hanoi 4 1 3
