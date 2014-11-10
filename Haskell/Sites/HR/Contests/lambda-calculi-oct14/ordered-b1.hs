{-# OPTIONS -O2 #-}

import Data.List

{-
data Sub = Sub {
 _result :: Int,
 _range :: (Int,Int)
} deriving (Show)
-}

main :: IO ()
main = do
 l <- getLine
 let (_N, _K) = (read (takeWhile (/= ' ') l) :: Int, read (dropWhile (/= ' ') l) :: Int)
 arr <- (getLine >>= \arr' -> return $ build'List arr')
-- return ()
 putStrLn $ show $ calc _N _K arr
-- putStrLn $ show _N ++ show _K ++ show arr

build'List s = read ("[" ++ (map (\n -> if n == ' ' then ',' else n) s) ++ "]") :: [Int]

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]

answer k results = head results

calc n k arr = calc' n k (0 : arr)

--calc' n k acc [] = acc
calc' n k arr = groupBy (\(x1,y1) (x2,y2) -> x1 == x2) $ sortBy (\(x1,y1) (x2,y2) -> compare x2 x1) $ filter (\(x,y) -> x>0) $ find'sums n k arr

find'sums n k arr = foldl' (\acc xs -> sum'fromRange arr (range xs) : acc) [] $ subseqs [1..(n-1)]

range [x] = (x,x)
range x = (head x, last x)

sum'fromRange arr (x,y) = (foldl (\acc idx -> acc + (arr !! idx)) 0 [x..y], (x,y))

t1 = calc 5 3 [2,4,-10,2,-2]
t2 = calc 4 2 [-2,5,-1,-8]
