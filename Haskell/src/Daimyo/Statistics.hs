{-# LANGUAGE RecordWildCards, ParallelListComp #-}

{-
    source: http://integral-table.com/downloads/stats.pdf
-}

module Daimyo.Statistics (
    Population,
    fromList,
    mean,
    mean'sample,
    variance,
    variance'sample,
    stddev,
    stddev'sample,
    zscore'sample,
    correlation'sample,
    µ,
    σ2,
    σ,
    xbar,
    s2x,
    sx,
    z,
    r,
    t_dogs
) where

import Data.List

data Population a = Population {
    set :: [a],
    n :: Double
} deriving (Show)

fromList l = Population {
    set = l,
    n = fromIntegral (length l) :: Double
}

mean Population{..} = (1/n) * sum set

mean'sample Population{..} = (1/n) * sum set

variance p@Population{..} =
    let
        smean = mean'sample p
        set' = map (\i -> (i - smean)**2) set
    in
        (1/n) * sum set'

variance'sample p@Population{..} =
    let
        smean = mean'sample p
        set' = map (\i -> (i - smean)**2) set
    in
        (1/(n-1)) * sum set'

stddev p@Population{..} =
    let
        var = variance p
    in
        sqrt var

stddev'sample p@Population{..} =
    let
        var = variance'sample p
    in
        sqrt var

zscore'sample x p@Population{..} = (x - (mean p)) / (stddev p) 

correlation'sample p q =
    (1/((n p)-1)) * (sum [ ((xi - (xbar p)) / (sx p)) * ((yi - (xbar p)) / (sx q)) | xi <- set p | yi <- set q])

(µ) = mean
(σ2) = variance
(σ) = stddev
xbar = mean'sample
s2x = variance'sample
sx = stddev'sample
z = zscore'sample
r = correlation'sample

{-
    dogs: http://www.mathsisfun.com/data/standard-deviation.html
-}

t_dogs = do
    let p = fromList [600,470,170,430,300]
    let q = fromList [601, 470, 160, 400, 350]
    putStrLn $ "population mean: " ++ (show $ mean p)
    putStrLn $ "population variance: " ++ (show $ variance p)
    putStrLn $ "population standard deviation: " ++ (show $ stddev p)
    putStrLn $ "sample mean: " ++ (show $ mean'sample p)
    putStrLn $ "sample variance: " ++ (show $ variance'sample p)
    putStrLn $ "sample standard deviation: " ++ (show $ stddev'sample p)
    putStrLn $ "sample z-score for 150: " ++ (show $ zscore'sample 150 p)
    putStrLn $ "sample correlation between p & q: " ++ (show $ correlation'sample p q)
