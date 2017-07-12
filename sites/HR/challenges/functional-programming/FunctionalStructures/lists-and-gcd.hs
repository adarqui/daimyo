import           Data.List

-- | lgcd
--
-- >>> lgcd [[(7,2)],[(2,2),(7,1)]]
-- [(7,1)]
--
-- >>> lgcd [[(2,2),(3,2),(5,3)],[(3,2),(5,3),(11,1)],[(2,2),(3,3),(5,4),(7,6),(19,18)],[(3,10),(5,15)]]
-- 3 2 5 3
--
lgcd :: [[(Int,Int)]] -> [(Int,Int)]
lgcd l = go (head sorted) (tail sorted)
  where
    go acc []   = acc
    go a (b:bs) = go [ (i, min j y) | (i,j) <- a, (x,y) <- b, i == x ] bs
    sorted      = sortByLength l

-- | sortByLength
--
-- >>> sortByLength [[(7,2)],[(2,2),(7,1)]]
-- [[(7,2)],[(2,2),(7,1)]]
--
-- >>> sortByLength [[(2,2),(3,2),(5,3)],[(3,2),(5,3),(11,1)],[(2,2),(3,3),(5,4),(7,6),(19,18)],[(3,10),(5,15)]]
-- [[(3,10),(5,15)],[(2,2),(3,2),(5,3)],[(3,2),(5,3),(11,1)],[(2,2),(3,3),(5,4),(7,6),(19,18)]]
--
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\xs ys -> compare (length xs) (length ys))

-- | listToTuples
--
-- >>> listToTuples [1,2,3,4]
-- [(1,2),(3,4)]
--
listToTuples :: [a] -> [(a,a)]
listToTuples []       = []
listToTuples (x:y:ys) = (x,y) : listToTuples ys

-- | tuplesToList
--
-- >>> tuplesToList [(1,2),(3,4)]
-- [1,2,3,4]
--
tuplesToList :: [(a,a)] -> [a]
tuplesToList []         = []
tuplesToList ((x,y):ys) = x : y : tuplesToList ys

-- | parseInput
--
-- >>> parseInput "7 2\n2 2 7 1\n"
-- [[(7,2)],[(2,2),(7,1)]]
--
parseInput :: String -> [[(Int, Int)]]
parseInput = map (listToTuples . map read . words) . lines

-- | solve
--
-- >>> solve [[(7,2)],[(2,2),(7,1)]]
-- "7 1"
--
solve :: [[(Int, Int)]] -> String
solve = concat . intersperse " " . map show . tuplesToList . lgcd

main :: IO ()
main = do
  _ <- getLine
  solution <- fmap parseInput getContents
  putStrLn $ solve solution
