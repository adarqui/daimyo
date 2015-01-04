module Daimyo.Matrix (
    size,
    det'2x2,
    is'2x2,
    abcd'2x2,
    inverse'2x2
) where

import Data.Matrix
import Data.Maybe

size m = (nrows m, ncols m)

det'2x2 m
    | is'2x2 m =
        let
            (a,b,c,d) = abcd'2x2 m
        in
            Just $ 1 / (a*d - b*c)
    | otherwise = Nothing

is'2x2 m
    | size m == (2,2) = True
    | otherwise = False

abcd'2x2 m =
    let
        a = m ! (1,1)
        b = m ! (1,2)
        c = m ! (2,1)
        d = m ! (2,2)
    in
        (a,b,c,d)

inverse'2x2 m
    | is'2x2 m && det'2x2 m /= Nothing =
        let
            (a,b,c,d) = abcd'2x2 m
            m' = fromList 2 2 [d,(-1)*b,(-1)*c,a]
        in
            Just $ scaleMatrix (fromJust (det'2x2 m)) m'
    | otherwise = Nothing

t_det_inverse'1 =
    let m = fromList 2 2 [11, -5, 2, -1] in (det'2x2 m, inverse'2x2 m)

t_det_inverse'2 =
    let m = fromList 2 2 [-3, 3, 8, 7] in (det'2x2 m, inverse'2x2 m)
