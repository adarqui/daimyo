module Daimyo.NumberTheory.Prime.Distance (
    distance,
    distances,
    distanceN,
    distancesN,
    dist,
    p1,
    p2,
    prime'distances,
    composite'distances,
    compositeOr1'distances,
    stats,
    t_stats
) where

import Daimyo.NumberTheory.Prime
import Daimyo.Statistics

distance = map dist distances
distances = map (\(p1,p2) -> (p1, p2, p2 - p1)) primes'pairs

distanceN n = map dist $ distancesN n
distancesN n = filter (\d -> dist d == n) distances

dist (_,_,d) = d
p1 (p1,_,_) = p1
p2 (_,p2,_) = p2

prime'distances = filter (\d -> isPrime $ dist d) distances
composite'distances = tail compositeOr1'distances
compositeOr1'distances = filter (\d -> not $ isPrime $ dist d) distances

stats l = fromListStats (map fromIntegral l)

t_stats n = stats (take n distance)

-- TODO
-- add max, min, sort, distribution/stats etc
