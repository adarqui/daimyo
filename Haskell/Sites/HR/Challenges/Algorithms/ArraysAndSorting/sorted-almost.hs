import Data.List

main :: IO ()
main = do
 _N <- readLn :: IO Int
 case (1 <= _N && _N <= (10^6)) of
  True -> do
   _P <- getLine
   putStrLn $ show $ as'str _N _P
  _ -> error "Constraint Error: 1 <= N <= 10^6)"

as'str n s = as n (read ("[" ++ (map (\n -> if n == ' ' then ',' else n) s) ++ "]") :: [Int])

as n p = 
 length $
  filter (\y -> isAs y) $
   subseqs $
    filter (\y -> y <= n) p

isAs xs =
 case xs of
  [] -> False
  [x] -> True
  _ -> first_and_last xs == (first_and_last $ sort xs)
 where
  first_and_last l = (head l, last l)

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]
