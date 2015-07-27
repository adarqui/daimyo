{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}

--
-- source: http://integral-table.com/downloads/stats.pdf
--

module Daimyo.Statistics (
  Population,
  PopulationStats,
  fromList,
  fromListStats,
  mean,
  meanSample,
  variance,
  varianceSample,
  stddev,
  stddevSample,
  zscoreSample,
  correlationSample,
  µ,
  σ2,
  σ,
  xbar,
  s2x,
  sx,
  z,
  r
) where

import           Data.List

-- | Population
--
data Population a = Population {
  popSet :: [a],
  popN   :: Double
} deriving (Show)

-- | PopulationStats
--
data PopulationStats a = PopulationStats {
  popStatsPop      :: Population a,
  popStatsMean     :: Double,
  popStatsVariance :: Double,
  popStatsStddev   :: Double
} deriving (Show)

-- | fromList
--
-- >>> fromList ([600, 470, 170, 430, 300] :: [Int])
-- Population {popSet = [600,470,170,430,300], popN = 5.0}
--
fromList :: [a] -> Population a
fromList l = Population {
  popSet = l,
  popN   = fromIntegral (length l) :: Double
}

-- | fromListStats
--
-- >>> fromListStats  ([600, 470, 170, 430, 300] :: [Double])
-- PopulationStats {popStatsPop = Population {popSet = [600.0,470.0,170.0,430.0,300.0], popN = 5.0}, popStatsMean = 394.0, popStatsVariance = 21704.0, popStatsStddev = 147.32277488562318}
--
fromListStats :: [Double] -> PopulationStats Double
fromListStats xs =
  PopulationStats {
    popStatsPop = pop,
    popStatsMean = mean pop,
    popStatsVariance = variance pop,
    popStatsStddev = stddev pop
  }
  where
    pop = fromList xs

-- | mean
--
mean :: Population Double -> Double
mean Population{..} = (1/popN) * sum popSet

-- | meanSample
--
meanSample :: Population Double -> Double
meanSample Population{..} = (1/popN) * sum popSet

-- | variance
--
variance :: Population Double -> Double
variance p@Population{..} = (1/popN) * sum set'
  where
    smean = meanSample p
    set'  = map (\i -> (i - smean)**2) popSet

-- | varianceSample
--
varianceSample :: Population Double -> Double
varianceSample p@Population{..} = (1/(popN-1)) * sum set'
  where
    smean = meanSample p
    set'  = map (\i -> (i - smean)**2) popSet

-- | stddev
--
stddev :: Population Double -> Double
stddev = sqrt . variance

-- | stddevSample
--
stddevSample :: Population Double -> Double
stddevSample = sqrt . varianceSample

-- | zscoreSample
--
zscoreSample :: Double -> Population Double -> Double
zscoreSample x p = (x - (mean p)) / (stddev p)

-- | correlationSample
--
correlationSample :: Population Double -> Population Double -> Double
correlationSample p q =
    (1/((popN p)-1)) * (sum [ ((xi - (xbar p)) / (sx p)) * ((yi - (xbar p)) / (sx q)) | xi <- popSet p | yi <- popSet q])

(µ) = mean
(σ2) = variance
(σ) = stddev
xbar = meanSample
s2x = varianceSample
sx = stddevSample
z = zscoreSample
r = correlationSample
