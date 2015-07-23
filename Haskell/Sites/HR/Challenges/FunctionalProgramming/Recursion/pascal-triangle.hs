import           Control.Monad
import           Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ solution (n-1)

-- | solution
--
solution :: Int -> String
solution = unlines . map format . pascalData 1 1

-- | format
--
-- >>> format [1,3,3,1]
-- "1 3 3 1"
--
format :: [Int] -> String
format = concat . intersperse " " . map show

-- | binomial
--
-- >>> binomial 5 5 5 :: [Int]
-- [3125,15625,31250,31250,15625,3125]
--
binomial :: Integral a => a -> a -> a -> [a]
binomial x y n = map formula [0..n]
  where
    formula k = (n `choose` k) * (x^(n-k))*(y^k)

-- | pascalData
--
-- >>> pascalData 5 5 5 :: [[Int]]
-- [[5,5],[25,50,25],[125,375,375,125],[625,2500,3750,2500,625],[3125,15625,31250,31250, 15625,3125]]
--
pascalData :: Integral a => a -> a -> a -> [[a]]
pascalData x y n = map (binomial x y) [1..n]

-- | choose
--
choose :: Integral a => a -> a -> a
n `choose` k = (permutations' (n, k)) `div` (factorial k)

-- | permutations
--
permutations' :: Integral s => (s, s) -> s
permutations' (m, n) = (factorial m) `div` (factorial $ m - n)

-- | factorial
--
factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
