{-# LANGUAGE RecordWildCards #-}

module Daimyo.Lib.Time (
    Per (..),
    hours,
    days
) where

data Per = PerSecond | PerHour | PerDay | PerWeek | PerMonth | PerYear deriving (Show, Read, Eq)

seconds t =
    case t of
        PerHour -> 3600.0
        PerDay -> 24.0 * seconds PerHour
        PerWeek -> 7.0 * seconds PerDay
        PerMonth -> 4.0 * seconds PerWeek
        PerYear -> 12.0 * seconds PerMonth

-- minutes =

hours t =
    case t of
        PerHour -> 1.0
        PerDay -> 24.0 * hours PerHour
        PerWeek -> 7 * hours PerDay
        PerMonth -> 30.4368 * hours PerDay
        PerYear -> 12.0 * hours PerMonth

days t =
    case t of
        PerSecond -> 1.15741e-5
        PerHour -> 0.0416667
        PerDay -> 1.0
        PerWeek -> 7.0 * days PerDay
        PerMonth -> 30.4368 * days PerDay
        PerYear -> 12.0 * days PerMonth

-- weeks

{-
    Use algebraic structures
-}
