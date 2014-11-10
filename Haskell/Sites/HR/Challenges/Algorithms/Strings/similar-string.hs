import Data.List
import Control.Monad

main :: IO ()
main = do
 _T <- readLn :: IO Int
 case (1 <= _T && _T <= 10) of
  True -> do
   strings <- mapM (\s -> getLine) [1.._T]
   mapM_ (putStrLn . show) $ find'similarities strings
  _ -> error "Constraint Error: 1 <= T <= 10"

find'similarities strings = map (\s -> sum $ find'similarity s) strings
find'similarity s = map (\suffix -> similarity suffix s) $ filter (\x -> head x == head s) $ suffixes s

suffixes [] = []
suffixes str@(s:ss) = str : suffixes ss

similarity pfx s = similarity' 0 pfx s

similarity' n [] _ = n
similarity' n (p:ps) (s:ss) =
 case (p == s) of
  True -> similarity' (n+1) ps ss
  _ -> n
