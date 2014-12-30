module Daimyo.Delrik (
    mxbn,
    mxbn',
    mxbn'',
    mxbn''',
    mxbn'''',
    euler'p1
) where

import Data.List

-- http://owp.ghost.io/project-euler-1/

mxbn :: Int -> Int -> Int -> [Int]
mxbn start x n = let 
    rp = start : (mxbn (start + x) x n) 
    in case (compare start n) of 
        LT -> rp 
        _ -> [0]


mxbn' :: Int -> Int -> Int -> [Int]
mxbn' start x n = let 
    rp = start : (mxbn (start + x) x n) 
    in case (compare start n) of 
        LT -> rp 
        _ -> []


mxbn''' start by end
    | start * by > end = []
    | otherwise = (start * by) : mxbn''' (start + 1) by end


mxbn'' :: Int -> Int -> Int -> [Int]
mxbn'' start by end = [ x | x <- [start,by..end], x < end ]


mxbn'''' start listby end =
    let
        multiples = map (\n -> mxbn'' start n end) listby
        duplicates = map (\n -> product listby * n) [start..end]
    in
        (concat multiples \\ duplicates)



euler'p1 multiples = sum $ nub $ concat multiples

t_mxbn3'5'1000 = euler'p1 [mxbn 0 3 1000, mxbn 0 5 1000]
t_mxbn'3'5'1000 = euler'p1 [mxbn' 0 3 1000, mxbn' 0 5 1000]
t_mxbn''3'5'1000 = euler'p1 [mxbn'' 0 3 1000, mxbn'' 0 5 1000]

t_mxbn''''3'5'1000 = sum $ mxbn'''' 0 [3,5] 1000
