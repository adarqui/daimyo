-- mostly taken from AAFA

module Daimyo.Set.Bit (
  Set,
  emptySet,
  isEmptySet,
  containedInSet,
  addSet,
  delSet,
  maxSet,
  toList
) where

import           Data.Maybe

-- | Set
--
newtype Set
  = Set Int
  deriving (Eq, Ord, Show)

-- | maxSet
--
maxSet :: Int
maxSet = truncate (logBase 2 (fromIntegral (maxBound :: Int)) :: Double) - 1

-- | emptySet
--
emptySet :: Set
emptySet = Set 0

-- | isEmptySet
--
isEmptySet :: Set -> Bool
isEmptySet (Set n) = n == 0

-- | containedInSet
--
containedInSet :: Int -> Set -> Maybe Bool
containedInSet i (Set set)
  | (i >= 0) && (i <= maxSet) = Just $ odd (set `div` (2^i))
  | otherwise                 = Nothing

-- | addSet
--
-- >>> addSet 62 $ fromJust $ addSet 6 $ fromJust $ addSet 5 $ emptySet
-- Just (Set 4611686018427388000)
addSet :: Int -> Set -> Maybe Set
addSet i (Set set)
  | (i >= 0) && (i <= maxSet) = Just $ Set (d' * e + m)
  | otherwise                 = Nothing
    where
      (d, m) = divMod set e
      e      = 2^i
      d'     = if odd d then d else d+1

-- | delSet
--
delSet :: Int -> Set -> Maybe Set
delSet i (Set set)
  | (i >= 0) && (i <= maxSet) = Just $ Set (d' * e + m)
  | otherwise                 = Nothing
    where
      (d, m) = divMod set e
      e      = 2^i
      d'     = if odd d then d-1 else d

-- | toList
--
-- >>> toList $ fromJust $ addSet 62 $ fromJust $ addSet 6 $ fromJust $ addSet 5 $ emptySet :: [Int]
-- [5,6,62]
--
toList :: Num t => Set -> [t]
toList (Set set) = go set 0
  where
    go 0 _ = []
    go n i
      | odd n     = i : go (n `div` 2) (i+1)
      | otherwise = go (n `div` 2) (i+1)
