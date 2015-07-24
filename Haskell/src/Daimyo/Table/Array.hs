module Daimyo.Table.Array (
  Table,
  newTable,
  findTable,
  updateTable
) where

import           GHC.Arr

-- | Table
--
newtype Table x i
  = Table (Array i x)
  deriving (Show)

-- | newTable
--
-- >>> newTable [(1, "a"), (2, "bb"), (3, "ccc")] :: Table String Int
-- Table (array (1,3) [(1,"a"),(2,"bb"),(3,"ccc")])
--
newTable :: Ix i => [(i,x)] -> Table x i
newTable assocs' = Table (array bounds' assocs')
  where
    indices' = map fst assocs'
    bounds'  = (minimum indices', maximum indices')

-- | findTable
--
-- >>> findTable 3 (newTable [(1, "a"), (2, "bb"), (3, "ccc")] :: Table String Int)
-- "ccc"
--
findTable :: Ix i => i -> Table x i -> x
findTable i (Table arr) = arr ! i

-- | updateTable
--
-- >>> findTable 3 $ updateTable (3, "hello") (newTable [(1, "a"), (2, "bb"), (3, "ccc")] :: Table String Int)
-- "hello"
--
updateTable :: Ix i => (i,x) -> Table x i -> Table x i
updateTable p (Table arr) = Table (arr // [p])
