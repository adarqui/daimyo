{-# OPTIONS -O2 #-}

import Data.List

main :: IO ()
main = do
 l <- getLine
 let (_N, _K) = (read (takeWhile (/= ' ') l) :: Int, read (dropWhile (/= ' ') l) :: Int)
 arr <- (getLine >>= \arr' -> return $ build'List arr')
 putStrLn $ show $ calc _N _K arr

build'List s = read ("[" ++ (map (\n -> if n == ' ' then ',' else n) s) ++ "]") :: [Int]

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]

answer k results = (maximumBy compare'sub $ head results, tail $ find'subs results)

calc n k arr = calc' n k (0 : arr)

-- reduces the results of find'sums
calc' n k arr = groupBy (\(x1,y1,z1) (x2,y2,z2) -> x1 == x2) $ sortBy (\(x1,y1,z1) (x2,y2,z2) -> compare x2 x1) $ filter (\(x,y,z) -> x>0) $ find'sums n k arr

find'sums n k arr = foldl' (\acc xs -> sum'fromRange arr (range xs) : acc) [] $ subseqs [1..(n-1)]

-- list to range
range [x] = (x,x)
range x = (head x, last x)

-- calculate the sum from an (x,y) range.. returns (sum,range,elements)
sum'fromRange arr (x,y) = (foldl (\acc idx -> acc + (arr !! idx)) 0 [x..y], (x,y), take ((y+1)-x) $ drop x arr)

-- compare sub based on the three rules: sum X > sum Y, sum X == sum Y && X begins earlier, sum X == sum Y && begin same && x ends earlier
compare'sub sub1@(x1,(y1a,y1b),z1) sub2@(x2,(y2a,y2b),z2)
 | x1 > x2 = GT
 | x1 == x2 && y1a < y2a = GT
 | x1 == x2 && y1a == y2a && y1b < y2b = GT
 | otherwise = LT

-- if elements of either list exist in both, return False
compare'elements'subs subs1@(x1,(y1a,y1b),z1) subs2@(x2,(y2a,y2b),z2) = compare'elements z2 z2
compare'elements [] _ = True
compare'elements elms1@(e:es) elms2 = case (any (\x -> e == x) elms2) of
 True -> False
 False -> compare'elements es elms2

-- find sub arrays disjoint to the previous.. takes a sorted/reduced list of subs
find'subs results = results
--find'subs acc subs = 

t1 = calc 5 3 [2,4,-10,2,-2]
t1f = find'sums 5 3 [2,4,-10,2,-2]
t1a = answer 3 t1
t2 = calc 4 2 [-2,5,-1,-8]
t2f = find'sums 4 2 [-2,5,-1,-8]


 
 
kadane :: [Integer] -> ( Integer , Int , Int )
kadane xs = ( sum , sIndex , eIndex )
 where
 (sum , _ , _ , _ , sIndex , eIndex ) = 
  foldl (\( mSum , cSum , sInd , eInd , sFin , eFin ) x ->
   if ( cSum + x > mSum )
   then ( cSum + x , cSum + x , sInd , eInd + 1 , sInd , eInd + 1 ) 
   else if ( cSum + x < 0 ) then ( mSum , 0 , eInd + 1 , eInd + 1 , sFin , eFin ) else ( mSum , cSum + x , sInd , eInd + 1, sFin , eFin  )  ) ( 0 , 0 , 0 , 0 , 0 , 0 ) xs
