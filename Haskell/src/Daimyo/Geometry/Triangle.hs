module Daimyo.Geometry.Triangle (
    Triangle (..),
    Side (..),
) where

data Side = A Double | B Double | C Double deriving (Show, Eq, Read)

data Triangle = Triangle {
    a :: Double,
    b :: Double,
    c :: Double
} deriving (Show, Eq)
