module Daimyo.LinearAlgebra.Quiz.Problems (
    p1
) where

import Data.Matrix

p1'equations =
    [
        "0x1 + 1x2 + 2x3 = 3",
        "1x1 + 2x2 + 3x3 = 0",
        "3x1 + 2x2 + 1x3 = 0"
    ]

p1'augmented'matrix =
    fromLists
        [
            [0,1,2,3],
            [1,2,3,0],
            [3,2,1,0]
        ]

p1'rref =
    let
        m = p1'augmented'matrix
    in
        m

p1 = undefined
