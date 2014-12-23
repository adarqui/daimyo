-- test

module Daimyo.Algebra.Quiz.ASMD (
) where

data ASMD a = ASMD {
    min :: a,
    max :: a,
    terms :: Int
} deriving (Show)
