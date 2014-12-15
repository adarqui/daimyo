module Daimyo.List.Single (
 List (..),
 head,
 tail,
 concat,
 append,
 prepend,
 cdr,
 cons,
 toList,
 fromList,
 map,
 mapM_,
 lines,
 filter
) where

import Data.List hiding (head, tail, concat, map, mapM_, lines, filter)
import Prelude hiding (head, tail, concat, map, mapM_, lines, filter)

data List a = Empty | Node a (List a) deriving (Eq, Show)

head :: List a -> a
head Empty = error "head of an empty list"
head (Node a r) = a

tail :: List a -> List a
tail Empty = error "tail of an empty list"
tail (Node _ r) = r

concat :: List a -> List a -> List a
concat Empty n = n
concat (Node a Empty) n = Node a n
concat (Node a r) n = Node a (concat r n)

append :: a -> List a -> List a
append a Empty = Node a Empty
append a (Node a' r') = Node a' (append a r')

prepend :: a -> List a -> List a
prepend a Empty = Node a Empty
prepend a r = Node a r

car = head
cdr = tail

cons :: a -> List a -> List a
cons a' Empty = Node a' Empty
cons a' (Node a r) = Node a' (Node a r)

toList :: List a -> [a]
toList Empty = []
toList (Node a r) = a : toList r

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Node x (fromList xs)

lines :: String -> List String
lines = lines' Empty

lines' :: List Char -> String -> List String
lines' acc [] = Node (toList acc) Empty
lines' acc (c:cs)
 | c == '\n' = Node (toList acc) (lines' Empty cs)
 | c /= '\n' = lines' (c `append` acc) cs

map :: (a -> b) -> List a -> List b
map f Empty = Empty
map f (Node a r) = Node (f a) (map f r)

mapM_ :: (a -> IO ()) -> List a -> IO ()
mapM_ f Empty = return ()
mapM_ f (Node a r) = f a >> mapM_ f r

filter :: (a -> Bool) -> List a -> List a
filter f Empty = Empty
filter f (Node a r) 
 | f a == True = Node a (filter f r)
 | otherwise = filter f r

t1 = Node 5 Empty
t2 = foldl' (\acc e -> acc `concat` Node e Empty) Empty [1..10]
t3 = cons 100 (cons 200 (cons 300 Empty))
t4 = concat t3 t2
