module Daimyo.Quiz (
    Quiz,
    Result (..)
) where

data Result = Pass | Fail deriving (Show, Read, Eq)

data Stats = Stats {
    pass :: Int,
    fail :: Int
} deriving (Show)

data Quiz a  = Quiz {
    stats :: Stats
} deriving (Show)

-- eh
