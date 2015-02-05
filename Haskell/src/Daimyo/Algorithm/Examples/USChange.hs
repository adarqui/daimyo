module Daimyo.Algorithm.Examples.USChange (
    change,
    uschange,
    uscoins
) where


change coins 0 = []
change coins m = 
    let
        c = head $ filter (<=m) coins
    in
        c : change coins (m-c)

uscoins = [25,10,5,1]

uschange = change uscoins
