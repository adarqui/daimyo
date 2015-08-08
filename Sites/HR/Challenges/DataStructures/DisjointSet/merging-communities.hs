{-# LANGUAGE ScopedTypeVariables #-}

{-
3 6
Q 1
M 1 2
Q 2
M 2 3
Q 3
Q 2
-}

import Data.Graph
import Control.Applicative
import Control.Monad.Trans.State

data Query = M Int Int | Q Int
  deriving (Eq, Show)

-- | parseQueries
--
-- >>> parseQueries ["Q 1", "M 1 2", "Q 2", "M 2 3", "Q 3", "Q 2"]
-- [Q 1,M 1 2,Q 2,M 2 3,Q 3,Q 2]
parseQueries :: [String] -> [Query]
parseQueries = map parseQuery

-- | parseQuery
--
-- >>> parseQuery "Q 1"
-- Q 1
--
-- >>> parseQuery "M 1 1"
-- M 1 1
parseQuery :: String -> Query
parseQuery all@(q:_)
  | q == 'Q'  = Q (read qPerson)
  | otherwise = M (read mPerson1) (read mPerson2)
  where
    fields = words all
    qPerson = fields !! 1
    (mPerson1, mPerson2) = (fields !! 1, fields !! 2)

-- | socialGraph
--
-- >>> socialGraph 3
-- array (1,3) [(1,[1]),(2,[2]),(3,[3])]
socialGraph :: Int -> Graph
socialGraph bound = buildG (1, bound) $ map (\n -> (n, n)) [1..bound]

-- shit.. i didn't know you couldn't 'insert an edge' after the graph is constructed

-- | runQueries
--
-- >>> runQueries 3 [Q 1,M 1 2,Q 2,M 2 3,Q 3,Q 2] (socialGraph 3)
-- ["1","2","3","3"]
runQueries :: Int -> [Query] -> Graph -> [String]
runQueries bound [] _ = []
runQueries bound (q:qs) g =
  case q of
    (Q person)          -> (show $ length $ reachable g person) : runQueries bound qs g
    (M person1 person2) -> runQueries bound qs (buildG (1, bound) (edges g ++ [(person1,person2), (person2,person1)]))

main :: IO ()
main = do
  params :: [Int] <- (map read . words) <$> getLine
  queries <- (parseQueries . lines) <$> getContents
  let
    (bound, query_tot) = (head params, last params)
    g = socialGraph bound
    output = runQueries bound queries g
  putStrLn $ unlines output
