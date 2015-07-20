{-
3
11 2 4
4 5 6
10 8 -12

=
15
-}

import Control.Applicative

-- | flatToMatrix
--
-- >>> flatToMatrix 3 [11,2,4,4,5,6,10,8,-12]
-- [[11,2,4],[4,5,6],[10,8,-12]]
flatToMatrix :: Int -> [Int] -> [[Int]]
flatToMatrix n_by_n xs = go xs
  where
    go [] = []
    go xs = take n_by_n xs : go (drop n_by_n xs)

-- | diagonals
--
-- >>> diagonals $ flatToMatrix 3 [11,2,4,4,5,6,10,8,-12]
-- ([11,5,-12],[4,5,10])
diagonals :: [[Int]] -> ([Int], [Int])
diagonals matrix = (go 0 matrix, go 0 (map reverse matrix))
  where
    go _ [] = []
    go n (x:xs) = (x !! n) : go (n+1) xs

-- | solution
--
-- >>> solution $ diagonals $ flatToMatrix 3 [11,2,4,4,5,6,10,8,-12]
-- 15
solution :: ([Int], [Int]) -> Int
solution (lr, rl) = abs $ sum lr - sum rl

main :: IO ()
main = do
  n_by_n <- readLn :: IO Int
  matrix <- (flatToMatrix n_by_n . map read . words) <$> getContents
  putStrLn $ show (solution $ diagonals matrix)
