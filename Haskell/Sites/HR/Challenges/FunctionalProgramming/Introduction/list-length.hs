{-# OPTIONS -O2 #-}

import Prelude hiding (filter, lines)

data List a = Empty | Node a (List a) deriving (Show)

size :: (Num b) => List a -> b
size Empty = 0
size (Node a r) = 1 + size r

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

toList :: List a -> [a]
toList Empty = []
toList (Node a r) = a : toList r

append :: a -> List a -> List a
append a Empty = Node a Empty
append a (Node a' r') = Node a' (append a r')

filter :: (a -> Bool) -> List a -> List a
filter f Empty = Empty
filter f (Node a r)
 | f a == True = Node a (filter f r)
 | otherwise = filter f r

lines = lines' Empty

lines' :: List Char -> String -> List String
lines' acc [] = Node (toList acc) Empty
lines' acc (c:cs)
 | c == '\n' = Node (toList acc) (lines' Empty cs)
 | c /= '\n' = lines' (c `append` acc) cs

main :: IO ()
main = do
 v <- getContents >>= \contents -> return $ lines contents
 let _N = size v
 case (0 <= _N && _N <= 100) of
  True -> putStrLn $ show $ size v
  False -> error "Constraint error: 0 <= N <= 100"
