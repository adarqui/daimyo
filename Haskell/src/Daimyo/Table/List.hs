module Daimyo.Table.List (
  Table,
  newTable,
  findTable,
  updateTable
) where

-- | Table
--
newtype Table value index
  = Table [(index, value)]
  deriving (Eq, Show)

-- | newTable
--
newTable :: (Eq index) => [(index,value)] -> Table value index
newTable = Table

-- | findTable
--
-- >>> findTable "a" $ newTable [("a",True),("b",False)]
-- Just True
--
-- >>> findTable "c" $ newTable [("a",True),("b",False)]
-- Nothing
--
findTable :: (Eq index) => index -> Table value index -> Maybe value
findTable _ (Table []) = Nothing
findTable i (Table ((i',v):r))
  | i == i'    = Just v
  | otherwise = findTable i (Table r)

-- | updateTable
--
updateTable :: (Eq index) => (index,value) -> Table value index -> Table value index
updateTable e (Table []) = Table [e]
updateTable a@(i,_) (Table (b@(j,_):r))
  | i == j = Table (a:r)
  | otherwise = Table (b:r')
  where
    Table r' = updateTable a (Table r)
