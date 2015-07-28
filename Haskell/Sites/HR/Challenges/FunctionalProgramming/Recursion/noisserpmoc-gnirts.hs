import           Data.Char
import           Data.List

-- | main
--
main :: IO ()
main = do
  msg <- getLine
  let msg_length = length msg
  if (all isLower msg && (msg_length >= 1 && msg_length <= (10^5)))
     then putStrLn $ concat $ map compress $ frequency msg
     else error "Constraint Error: 1 <= length(msg) <= 10^5 && msg consists of lowercase Latin characters ('a'-'z') only."

-- | compress
--
-- >>> compress (5, 'a')
-- "a5"
--
compress :: (Show a, Ord a, Num a) => (a, Char) -> [Char]
compress (n, c) =
  if n <= 1
     then [c]
     else [c] ++ show n

-- | frequency
--
-- >>> frequency "abbcccdddd"
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
--
frequency :: (Eq a) => [a] -> [(Int,a)]
frequency [] = []
frequency (x:xs) = (l, x) : (frequency r)
  where
    l = succ $ length $ takeWhile (== x) xs
    r = dropWhile (== x) xs
