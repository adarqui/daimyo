module Daimyo.List.Pattern (
    rotations
) where

rotations :: [a] -> [[a]]
rotations l = rotations' (length l) l

rotations' rotation (x:xs)
    | rotation <= 0 = []
    | otherwise =
        let
            new = (xs ++ [x])
        in
            new : rotations' (rotation-1) new
