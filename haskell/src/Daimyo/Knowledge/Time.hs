{-# LANGUAGE RecordWildCards #-}

module Daimyo.Knowledge.Time (
) where

data Hours = PerSecond | PerHour | PerDay | PerWeek | PerMonth | PerYear deriving (Show, Read, Eq)

data LearnLife = LearnLife {
    avgWorkHoursPerDay :: Double,
    avgSleepHoursPerNight :: Double
} deriving (Show, Read, Eq)

build w s = LearnLife w s

{-
    lrn = 
numbers LearnLife{..} =
    map (\i -> 
-}

hours t =
    case t of
        PerHour -> 1.0
        PerDay -> 24.0
        PerWeek -> 168.0
        PerMonth -> 730.484
        PerYear -> 8765.81

days t =
    case t of
        PerHour -> 0.0416667
        PerDay -> 1.0
        PerWeek -> 7.0
        PerMonth -> 30.4368
        PerYear -> 365.242
