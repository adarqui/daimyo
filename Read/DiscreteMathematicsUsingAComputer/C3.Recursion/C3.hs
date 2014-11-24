{-# LANGUAGE NPlusKPatterns #-}

import Data.List

factNK :: Int -> Int
factNK 0 = 1
factNK (n+1) = (n+1) * factNK n

factProd n = product [1..n]

factFold n = foldl' (*) 1 [1..n]

len [] = 0
len (h:t) = 1 + len t

sum' [] = 0
sum' (h:t) = h + sum' t

[] `join` ys = ys
(x:xs) `join` ys = x : (xs `join` ys)

zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

qsort [] = []
qsort (splitter:xs) = qsort (filter (<splitter) xs) ++ [splitter] ++ qsort (filter (>=splitter) xs)

copy [] = []
copy (h:t) = h : copy t

inverse [] = []
inverse ((x,y):t) = (y,x) : inverse t

{- lazy
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = 
-}
merge a b = qsort $ a ++ b

(!!!) [] _ = Nothing
(!!!) (x:_) 0 = Just x
(!!!) (_:xs) n = (!!!) xs (n-1)

lookup' n [] = Nothing
lookup' n ((x,y):xs)
    | x == n = Just y
    | otherwise = lookup' n  xs

count e [] = 0
count e (h:t) = (if (e == h) then 1 else 0) + count e t

drope e [] = []
drope e (x:xs) 
    | e == x = drope e xs
    | otherwise = x : drope e xs

dropAlt [] = []
dropAlt (h:t) = dropAlt' t
dropAlt' [] = []
dropAlt' (h:t) = h : dropAlt t

extract [] = []
extract (x:xs) =
    case x of
    Nothing -> extract xs
    (Just x') -> x' : extract xs
    
map' f [] = []
map' f (x:xs) = f x : map f xs

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

foldR f z [] = z
foldR f z (x:xs) = f x (foldr f z xs)

sumR = foldr (+) 0
productR = foldr (*) 1
and = foldr (&&) True
or = foldr (||) False
factorial n = foldr (*) 1 [1..n]

foldrWith a f z [] = z
foldrWith a f z (x:xs) = f a x (foldrWith a f z xs)

mappend f xs = concat (map f xs)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = noConsecutives . qsort

noConsecutives :: Ord a => [a] -> [a]
noConsecutives [] = []
noConsecutives (x:xs) = x : (noConsecutives $ dropWhile (==x) xs)

elem' e [] = False
elem' e (x:xs)
    | e == x = True
    | otherwise = False


data Peano = Zero | Succ Peano deriving (Show)

one = Succ Zero
three = Succ (Succ (Succ Zero))

decr Zero = Zero
decr (Succ a) = a

add Zero b = b
add (Succ a) b = Succ (add a b)

sub a Zero = a
sub Zero _ = Zero
sub (Succ a) (Succ b) = sub a b

equals Zero Zero = True
equals Zero _ = False
equals _ Zero = False
equals (Succ a) (Succ b) = equals a b

lt a Zero = False
lt Zero (Succ _) = True
lt (Succ a) (Succ b) = lt a b

f :: a -> [a]
f x = x : f x

ones = f 1

twos = 2 : twos


graph =
    let
        a = 1 : b
        b = 2 : c
        c = [3] ++ a
    in
        a

{- Examples -}

ex1 = copy [2]
ex2 = inverse [(1,2),(3,4)]
ex3 = merge [1,2,5,201,3,4,2,1] [2,4,8,5,4,6,2]
ex4 = [l !!! 0, l !!! 2, l !!! 5]
    where
        l = [1,2,3]
ex5 = [lookup' 5 l, lookup' 6 l]
    where
        l = [(1,2),(5,3)]
ex6 = count 1 [1,2,3,1,2,3,1]
ex7 = drope 1 [1,2,3,1,2,3,1]
ex8 = dropAlt [1..7]
ex9 = extract [Just 3, Nothing, Just 7]
ex13 = removeDuplicates [1,2,3,1,1,2,3,4,2,9]
ex14 = (elem' 1 [0,2,3,4], elem' 1 [1,2])

