{-# OPTIONS_GHC -O2 #-}
data Tree k v = Empty | Node k v (Tree k v) (Tree k v) deriving (Show)

insert :: (Ord k, Eq k, Num v) => k -> Tree k v -> Tree k v
insert k Empty = Node k 1 Empty Empty
insert k (Node k' v' l r)
 | k' == k = Node k' (v'+1) l r
 | k' < k = Node k' v' (insert k l) r
 | k' > k = Node k' v' l (insert k r)

fromList :: (Ord k, Num v) => [k] -> Tree k v
fromList [] = Empty
fromList (k:ks) = insert k (fromList ks)

uniques :: (Ord k, Ord v, Num v) => Tree k v -> [k]
uniques Empty = []
uniques (Node k v l r)
 | v == 1 = k : uniques l ++ uniques r
 | otherwise = uniques l ++ uniques r

intersperse :: a -> [a] -> [a]
intersperse e [] = []
intersperse e (x:xs) = x : e : intersperse e xs

-- lonely-integer

main :: IO ()
main = do
 (_N, _A) <- input
 case (_N `rem` 2 > 0) of
  True -> putStrLn $ output $ uniques $ fromList _A
  False -> error "Constraint error: N is not odd"
 
input = do
 _N <- readLn :: IO Int
 getLine >>= \line -> return (_N, map (\w -> read w :: Int) $ words line)

output _A = concat $ intersperse " " $ map (\n -> show n) _A
