module Daimyo.Table.Naive (
  Table,
  newTable,
  findTable,
  updateTable
) where

-- | Table
--
newtype Table value index = Table (index -> value)

instance Show (Table value index) where
  showsPrec _ _ str = showString "<<Table>>" str

-- | newTable
--
newTable :: (Eq index) => [(index,value)] -> Table value index
newTable assocs = foldr updateTable (Table (\_ -> error "Not Found")) assocs

-- | findTable
--
-- >>> findTable "a" $ newTable [("a",True),("b",False)]
-- True
--
findTable :: (Eq index) => index -> Table value index -> value
findTable i (Table f) = f i

-- | updateTable
--
updateTable :: (Eq index) => (index,value) -> Table value index -> Table value index
updateTable (i,x) (Table f) = Table g
  where
    g j
      | j == i    = x
      | otherwise = f j
