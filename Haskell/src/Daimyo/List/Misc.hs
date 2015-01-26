module Daimyo.List.Misc (
    subseqs,
    subsequences',
    unique'unordered'subsequences,
    interleave,
    rests,
    rests'test,
    removeIndices,
    choiceList,
    mapIf,
    foldlIf
) where

import Data.List

subseqs :: [a] -> [[a]]
subseqs l = [x | i <- inits l, x <- tails i, not $ null x ]

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

subsequences' l = filter (not . null) $ subsequences l

unique'unordered'subsequences l = nub $ map sort $ subsequences l

{-
    rests
    [2,2,2,3]
    ([2],[2,2,3])
    ([2,2],[2,3])
    ([2,3],[2,2])
    ([2,2,2],[3])
    ..
-}

rests l =
    let
        perms = permutations l
    in
        nub $ concatMap (\p -> map (\n -> (take n p, drop n p)) [1..(length l)-1]) perms

rests'test l =
    let
        subs = subseqs l
    in
        nub $ concatMap (\p -> map (\n -> (take n p, drop n p)) [1..(length l)-1]) subs



{-
    Removes elements found in the list of indexes
    Returns a tuple: (removed items, new list)
-}

removeIndices :: [Int] -> [a] -> ([a],[a])
removeIndices indices list = removeIndices' 0 ([],[]) (sort $ nub indices) list

removeIndices' :: Int -> ([a],[a]) -> [Int] -> [a] -> ([a], [a])
removeIndices' _ (acc,acc') [] list = (acc,acc' ++ list)
removeIndices' _ (acc,acc') _ [] = (acc,acc')
removeIndices' iter (acc,acc') indices@(i:is) list@(x:xs)
    | iter == i = removeIndices'(iter+1) (acc++[x],acc') is xs
    | otherwise = removeIndices' (iter+1) (acc,acc'++[x]) indices xs
    
t_removeIndieces =
    let
        s = "hello world"
        ri = removeIndices
    in
        [ri [0,1,2] s, ri [1,3] s, ri [6] s, ri [] s, ri [100] s, ri [100] []]


{-
    choice list
    [[1,2],[3,4],[5,6],[7,8]]
    1 3 5 7
    1 3 5 8
    2 3 5 7
    1 4 5 7
    2 4 5 7
    ...
-}

choiceList = undefined


mapIf t aTob a = takeWhile t $ map aTob a


foldlIf _ _ b [] = (True, b)
foldlIf t bToaTob b (a:as) =
    let
        r = bToaTob b a
    in
        if (t r)
            then foldlIf t bToaTob r as
            else (False, b)


t_mapIf = [mapIf (>3) (+1) [1..10], mapIf (>0) (+1) [1..10]]
t_foldlIf = [foldlIf (>0) (\acc x -> x + acc) 0 [0..10], foldlIf (>0) (\acc x -> x + acc) 0 [1..10]]
