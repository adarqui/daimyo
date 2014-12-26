{-# LANGUAGE RecordWildCards #-}

module Daimyo.Geometry.Line (
    module Daimyo.Geometry.Point,
    Line,
    new,
    slope,
    parallel
) where

import Daimyo.Geometry.Point
import Data.Ratio

data Line = Line {
    a :: Point,
    b :: Point,
    m :: Ratio Integer
} deriving (Show, Read, Eq)

new p1 p2 = Line {
    a = p1,
    b = p2,
    m = slope' p1 p2
}

{-
    y = mx + b

    m = (y2 - y1) / (x2 - x1)
-}

slope l = slope' (a l) (b l)

slope' p1 p2 =
    let
        (x1, y1) = (x p1, y p1)
        (x2, y2) = (x p2, y p2)
    in
        toRational $ (y2 - y1) / (x2 - x1)

parallel l1 l2 = m l1 == m l2

t_line'1 = new (Point 1 2) (Point 3 4)
t_line'2 = new (Point (-3) 4) (Point 5 4)
t_line'3 = new (Point (-1) 5) (Point 2 (-1))
t_line'4 = new (Point 1 2) (Point 3 5)
