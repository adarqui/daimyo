module Daimyo.Geometry.Point (
    Point (..)
) where

data Point = Point {
    x :: Double,
    y :: Double
} deriving (Show, Read, Eq)
