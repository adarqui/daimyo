{-# LANGUAGE RankNTypes #-}
module Daimyo.Lib.Sorting.OddEven (
 oesort,
 fromList'ST',
 toList'ST'
) where

import GHC.Arr
import GHC.ST
import Control.Monad.ST

-- Using lists
oesort :: (Ord a) => [a] -> [a]
oesort [] = []
oesort r = oesort' True r

oesort' False r = r
oesort' _ r= oesort' b' r'
 where
  (e:vens,b) = oesort'swap ([],False) r
  (r'@(o:dds),b') = oesort'swap ([e],b) vens

oesort'swap (acc,b) [] = (acc,b)
oesort'swap (acc,b) (x:[]) = (acc++[x],b)
oesort'swap (acc,b) (x:y:z)
 | x > y = oesort'swap (acc++[y,x],True) z
 | otherwise = oesort'swap (acc++[x,y],b) z

t1 = oesort [1,4,2,4,9,2,4,2,1,1,100,2,4,2,1,4,6,7,4,2,2,5,7,4,2,1,4,6,7,8,8,99,100,1000,200,55]


-- Using array

oesort'ST l =
 runSTArray $ do
  st <- newSTArray (0,sz-1) 0
  fromList'ST' l 0 st
  go True st
  return st
 where
  sz = length l
  go sorted st = do
   sorted' <- oesort'ST'swap sorted st 1 (sz-1)
   sorted'' <- oesort'ST'swap sorted' st 0 (sz-1)
   if (sorted'' == False)
    then go True st
    else return ()

oesort'ST'swap sorted st i j | (j-1) < i = return sorted
oesort'ST'swap sorted st i j = do
 m <- readSTArray st i
 n <- readSTArray st (i+1)
 if (m > n)
  then do
   swap st (m,i) (n,i+1)
   oesort'ST'swap False st (i+2) j
  else oesort'ST'swap sorted st (i+2) j
   
swap st (m,i) (n,j) = do
 writeSTArray st i n
 writeSTArray st j m

fromList'ST' [] _ st = return ()
fromList'ST' (x:xs) i st = writeSTArray st i x >> fromList'ST' xs (i+1) st

--staToList' st acc = 
toList'ST' st i j = [ readSTArray st k | k <- [i..j] ]

runSTArray :: (Ix i)
	   => (forall s . ST s (STArray s i e))
	   -> Array i e
runSTArray st = runST (st >>= unsafeFreezeSTArray)

--staFromList (x:xs) i st = writeSTArray st i x >> staFromList xs (i+1) st


-- TODO


{- ------------- old stuff, for fun.. ------------- -}
oesort'old1 :: (Ord a) => [a] -> [a]
oesort'old1 [] = []
oesort'old1 l@(x:xs) =
 case b' of
  True -> oesort'old1 r
  False -> r
 where
  (e:vens,b) = oesort'old1'swap ([],False) l
  (r@(o:dds),b') = oesort'old1'swap ([e],b) vens

oesort'old1'swap (acc,b) [] = (acc,b)
oesort'old1'swap (acc,b) (x:[]) = (acc++[x],b)
oesort'old1'swap (acc,b) (x:y:z)
 | x > y = oesort'old1'swap (acc++[y,x],True) z
 | otherwise = oesort'old1'swap (acc++[x,y],b) z


oesort'old2 :: (Ord a) => [a] -> [a]
oesort'old2 [] = []
oesort'old2 l@(x:xs) = f' r
 where
  (e:vens,f) = oesort'old2'swap ([],oesort'old2'done) l
  (r@(o:dds),f') = oesort'old2'swap ([e],f) vens

oesort'old2'done r = r

oesort'old2'swap (acc,b) [] = (acc,b)
oesort'old2'swap (acc,b) (x:[]) = (acc++[x],b)
oesort'old2'swap (acc,b) (x:y:z)
 | x > y = oesort'old2'swap (acc++[y,x],oesort'old2) z
 | otherwise = oesort'old2'swap (acc++[x,y],b) z
