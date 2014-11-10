import Data.List
import Data.Char

main :: IO ()
main = do
 msg <- getLine
 let msg'len = length msg
 case (all isLower msg && (msg'len >= 1 && msg'len <= (10^5))) of
  True -> putStrLn $ concat $ map compress $ frequency msg
  _ -> error "Constraint Error: 1 <= length(msg) <= 10^5 && msg consists of lowercase Latin characters ('a'-'z') only."
 return ()

compress (n, c) =
 case (n <= 1) of
  True -> [c]
  _ -> [c] ++ show n

frequency :: (Eq a) => [a] -> [(Int,a)]
frequency [] = []
frequency (x:xs) = (half'fst, x) : (frequency half'snd)
 where
  half'fst = succ $ length $ takeWhile (== x) xs
  half'snd = dropWhile (== x) xs
