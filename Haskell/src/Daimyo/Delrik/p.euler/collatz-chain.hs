module 
    Collatz(chain) 
where

chain :: Int -> [Int]
chain 1 = [1]
chain n
    | even n =  n : chain (n `div` 2)
    | odd n  =  n : chain (n * 3 + 1)

elrk_elem :: (Eq a) => a -> [a] -> Bool
elrk_elem a [] = False
elrk_elem e (xs) = foldr check False xs
    where
        check term found = if term == e then True else found
