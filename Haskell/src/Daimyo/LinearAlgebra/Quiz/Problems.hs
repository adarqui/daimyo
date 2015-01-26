module Daimyo.LinearAlgebra.Quiz.Problems (
    p1
) where

import Daimyo.LinearAlgebra.Other.GaussJordan
import Data.Matrix
import Control.Monad

p1'equations =
    [
        "0x1 + 1x2 + 2x3 = 3",
        "1x1 + 2x2 + 3x3 = 0",
        "3x1 + 2x2 + 1x3 = 0"
    ]


p1'gj =
    let
        m = [[0,1,2],[1,2,3],[3,2,1]]
        v = [3,0,0]
    in
        gauss m v
        

p1'lu = luDecomp p1'augmented'matrix


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
        m'a = switchRows 1 2 m
        m'b = combineRows 1 (-2) 2 m'a
        m'c = combineRows 3 (-3) 1 m'b
        m'd = combineRows 3 (-2) 2 m'c
    in
        [("augmented",m), ("swap R1,R2",m'a), ("R1= -2*R2+R1",m'b), ("R3= -3*R1+R3",m'c), ("R3= -2*R2+R3",m'd)]


p1 =
    let
        rref = p1'rref
    in
        mapM_ (\(s,m) -> putStrLn ("Operation: " ++ s) >> putStrLn (show m)) rref

