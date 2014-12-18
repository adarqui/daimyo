module Daimyo.String (
    split
) where

split c s = split' [] c s

split' acc c [] = ([], [])
split' acc c (x:xs)
    | c == x = (acc, xs)
    | otherwise = split' (acc++[x]) c xs
