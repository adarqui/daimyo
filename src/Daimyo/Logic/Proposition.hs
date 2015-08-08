module Daimyo.Logic.Proposition (
    Prop (..)
) where

data Prop = T | F deriving (Show, Read, Eq, Enum)
