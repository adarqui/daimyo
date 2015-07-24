{-# LANGUAGE NoImplicitPrelude #-}

module Daimyo.EquationalReasoning.Data.List.Permutations (
) where

-- Pre-requisites
--

infixr 9 .

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

id :: a -> a
id a = a

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

permutations'            :: [a] -> [[a]]
permutations' xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr (\xs r -> interleave xs r) (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

{-
 - start with something simpler :D

permutations_eqr1 "abc" = "abc" : perms1 "abc" []
  where
    perms1 []     _      = []
    perms1 ('a':"bc") [] = foldr (\xs r -> interleave xs r) (perms2 "bc" ('a':[])) (permutations_eqr2 [])
    perms2 []     _      = []
    perms2 ('b':"c") "a" = foldr (\xs r -> interleave xs r) (perms3 "c" ('b':"a")) (permutations_eqr3 "a")
    perms2 []     _      = []
    perms2 ('c':[]) "b"  = foldr (\xs r -> interleave xs r) (perms3 [] ('c':"b")) (permutations_eqr4 "b")
    perms3 []     _      = []
--  permsX []     _      = []
--  permsX (t:ts) is     = foldr (\xs r -> interleave xs r) (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = ("bc", r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f ('a':y:us) : zs)

permutations_eqr2 [] = []

permutations_eqr3 "a" = "a" : perms1 "a" []
  where
    perms1 []     _      = []
    perms1 ('a':[]) [] = foldr (\xs r -> interleave xs r) (perms2 [] ('a':[])) (permutations_eqr2 [])
    perms2 []     _      = []
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = ([], r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f ('a':y:us) : zs)

permutations_eqr3 "b" = "b" : perms1 "b" []
  where
    perms1 []     _      = []
    perms1 ('b':[]) [] = foldr (\xs r -> interleave xs r) (perms2 [] ('b':[])) (permutations_eqr2 [])
    perms2 []     _      = []
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = ([], r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f ('b':y:us) : zs)
-}
