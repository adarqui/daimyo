module Daimyo.List.Safe (
    safeHead
) where

{-
    head of empty list is Nothing
-}

safeHead [] = Nothing
safeHead l = Just $ head l
