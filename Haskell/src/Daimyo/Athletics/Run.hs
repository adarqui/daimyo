module Daimyo.Athletics.Run (
    mi'to'km,
    km'to'mi,
    minsec'to'sec,
    sec'to'minsec
) where

-- some sloppy quick calculations

mi'to'km mi = mi * 1.60934
km'to'mi km = km * 0.621371

minsec'to'sec (min,sec) = min*60 + sec
sec'to'minsec s = quotRem s 60
