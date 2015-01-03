module Daimyo.Athletics.Run (
    mi'to'k,
    k'to'mi,
    minsec'to'sec,
    sec'to'minsec
) where

-- some sloppy quick calculations

mi'to'k mi = mi * 1.60934
k'to'mi k = k * 0.621371

minsec'to'sec (min,sec) = min*60 + sec
sec'to'minsec s = quotRem s 60
