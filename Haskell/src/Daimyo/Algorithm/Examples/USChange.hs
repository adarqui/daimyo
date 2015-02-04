module Daimyo.Algorithm.Examples.USChange (
    uschange
) where

coins = [25,10,5,1]

uschange 0 = []
uschange m = 
    let
        c = head $ filter (<=m) coins
    in
        c : uschange (m-c)
