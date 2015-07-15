{-# OPTIONS -O2 #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.List

data TCase = TCase {
 _n     :: Integer,
 _alpha :: Integer,
 _beta  :: Integer,
 _nums  :: [Integer]
} deriving (Show)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)


fromListL :: (Ord a) => [a] -> Tree a
fromListL l = foldl' (\acc e -> insL e acc) Empty l

fromListR :: (Ord a) => [a] -> Tree a
fromListR l = foldl' (\acc e -> insR e acc) Empty l

insL :: (Ord a) => a -> Tree a -> Tree a
insL e Empty = Node e Empty Empty
insL e (Node a lb rb)
 | e <= a = Node a (insL e lb) rb
 | otherwise = Node a lb (insL e rb)

insR :: (Ord a) => a -> Tree a -> Tree a
insR e Empty = Node e Empty Empty
insR e (Node a lb rb)
 | e < a = Node a (insR e lb) rb
 | otherwise = Node a lb (insR e rb)

size :: Tree a -> Int
size Empty = 0
size (Node a lb rb) = 1 + (size lb) + (size rb)

sumT :: Tree Integer -> (Integer,Integer)
sumT = sumT' 0 (0,0)

-- esum = even sum, osum = odd sum
sumT' :: Int -> (Integer,Integer) -> Tree Integer -> (Integer,Integer)
sumT' levle (esum,osum) Empty = (0,0)

takeLevel :: (Int -> Bool) -> Tree a -> [a]
takeLevel f t = takeLevel' f 0 t

takeLevel' :: (Int -> Bool) -> Int -> Tree a -> [a]
takeLevel' f level Empty = []
takeLevel' f level (Node a lb rb) =
 if (f level)
  then a : takeLevel' f (level+1) lb ++ takeLevel' f (level+1) rb
  else takeLevel' f (level+1) lb ++ takeLevel' f (level+1) rb

takeLevelSum f t = sum $ takeLevel f t

pairAdd (a,b) (c,d) = (a+c,b+d)

pp :: (Show a) => Tree a -> IO ()
pp t = do
 putStrLn "\n--------------------------\n"
 mapM_ putStrLn $ mapIndent t
 where
  mapIndent Empty          = [""]
  mapIndent (Node a lb rb) =
   ["--(" ++ (show a) ++ ")"] ++
   map ("  |" ++) ls ++
   ("  `" ++ r) : map ("   " ++) rs
    where
     (r:rs) = mapIndent $ rb
     ls     = mapIndent $ lb

main :: IO ()
main = do
 _T <- readLn :: IO Integer
 tcases <- mapM (\_ -> readTCase) [1.._T]
 mapM_ (\r -> putStrLn $ show $ solve r) tcases

solve' t = map (\t' -> (takeLevel (\n -> n `mod` 2 == 0) t', takeLevel (\n -> n `mod` 2 /= 0) t')) $ filt t
solve'' t = map (\t' -> (takeLevelSum (\n -> n `mod` 2 == 0) t', takeLevelSum (\n -> n `mod` 2 /= 0) t')) $ filt t

solve t@TCase{..} =
 (sum $ map (\t -> liking t (_alpha,_beta)) sums) `mod` modp
 where
  sums = map (\t' ->(takeLevelSum (\n -> n `mod` 2 == 0) t', takeLevelSum (\n -> n `mod` 2 /= 0) t')) $ filt t

pretty t = mapM_ pp $ filt t

filt TCase{..} = filter (\t -> size t == length _nums) trees
 where
  trees = nub $ map fromListL perms ++ map fromListR perms
  perms = permutations _nums

modp = 10^9

liking (e,o) (alpha,beta) = (e * alpha) - (o * beta)

readTCase = do
 _N <-readLn :: IO Integer
 (alpha,beta) <- getLine >>= \line -> do
  let w = words line
  return (read (w !! 0) :: Integer, read (w !! 1) :: Integer)
 nums <- getLine >>= \line -> return $ map (\l -> read l :: Integer) $ words line
 return TCase {
  _n = _N,
  _alpha = alpha,
  _beta = beta,
  _nums = nums
 }

tcase n alpha beta nums = TCase { _n = n, _alpha = alpha, _beta = beta, _nums = nums }

t1 = tcase 1 1 1 [1]
t2 = tcase 2 1 1 [1,2]
t3 = tcase 3 1 1 [1,2,3]
t4 = tcase 5 1 1 [1,2,3,4,5]
t5 = tcase 2 1 1 [2,2]
