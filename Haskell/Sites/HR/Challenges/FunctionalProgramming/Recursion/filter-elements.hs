{-# LANGUAGE ScopedTypeVariables #-}

import           Data.List hiding (filter, find, insert, union)
import           Control.Monad
import           Data.Maybe

-- | BST
--
data BST k v
  = Empty
  | Node k v (BST k v) (BST k v)
  deriving (Eq, Show)

-- | size
--
size :: (Num a) => BST k v -> a
size Empty          = 0
size (Node _ v l r) = 1 + size l + size r

-- | fromList
--
-- >>> fromList ([(1,2),(3,4),(1,3),(6,7),(8,1),(4,9),(3,5)] :: [(Int, Int)])
-- Node 1 3 Empty (Node 3 5 Empty (Node 6 7 (Node 4 9 Empty Empty) (Node 8 1 Empty Empty)))
--
fromList :: Ord k => [(k,v)] -> BST k v
--fromList = foldl' (\acc (k,v) -> update k v acc) Empty
fromList = fromListWith withKvNew

-- | fromListWith
--
-- >>> fromListWith withKvOld ([(1,2),(3,4),(1,3),(6,7),(8,1),(4,9),(3,5)] :: [(Int, Int)])
-- Node 1 2 Empty (Node 3 4 Empty (Node 6 7 (Node 4 9 Empty Empty) (Node 8 1 Empty Empty)))
--
fromListWith :: Ord k => (k -> v -> v -> v) -> [(k,v)] -> BST k v
fromListWith with = foldl' (\acc (k,v) -> updateWith with k v acc) Empty

-- | toList
--
-- >>> toList $ fromList ([(1,2),(3,4),(1,3),(6,7),(8,1),(4,9)] :: [(Int, Int)])
-- [(4,9),(1,2),(3,4),(8,1),(6,7)]
--
toList :: BST k v -> [(k,v)]
toList = preOrder

-- | preOrder
--
preOrder :: BST k v -> [(k,v)]
preOrder Empty          = []
preOrder (Node k v l r) = (k,v) : preOrder l ++ preOrder r

-- | inOrder
--
inOrder :: BST k v -> [(k,v)]
inOrder Empty          = []
inOrder (Node k v l r) = inOrder l ++ [(k,v)] ++ inOrder r

-- | postOrder
postOrder :: BST k v -> [(k,v)]
postOrder Empty          = []
postOrder (Node k v l r) = postOrder l ++ postOrder r ++ [(k,v)]

-- | insert
--
insert :: Ord k => k -> v -> BST k v -> BST k v
insert = update

-- | remove
--
remove :: (Eq k, Ord k) => k -> BST k v -> BST k v
remove k Empty = Empty
remove k' (Node k v l r)
 | k' == k = Empty
 | k' < k  = Node k v (remove k l) r
 | k' > k  = Node k v l (remove k r)

-- | update
--
-- >>> update 1 4 $ fromList ([(1,2),(3,4),(1,3),(3,5),(6,7)] :: [(Int, Int)])
-- Node 1 4 Empty (Node 3 5 Empty (Node 6 7 Empty Empty))
--
update :: Ord k => k -> v -> BST k v -> BST k v
update = updateWith withKvNew

-- | withKvOld
--
withKvOld :: a -> b -> c -> b
withKvOld _ old _ = old

-- | withKvNew
--
withKvNew :: a -> b -> c -> c
withKvNew _ _ new = new

-- | updateWith
--
updateWith :: Ord k => (k -> v -> v -> v) -> k -> v -> BST k v -> BST k v
updateWith with k v Empty = Node k v Empty Empty
updateWith with k v (Node k' v' l' r')
 | k == k' = Node k' (with k' v' v) l' r'
 | k < k'  = Node k' v' (updateWith with k v l') r'
 | k > k'  = Node k' v' l' (updateWith with k v r')

-- | find
--
-- >>> find 3 $ update 1 4 $ fromList ([(1,2),(3,4),(1,3),(3,5),(6,7)] :: [(Int, Int)])
-- Just 4
--
find :: Ord k => k -> BST k v -> Maybe v
find _ Empty = Nothing
find k' (Node k v l r)
 | k' == k = Just v
 | k' < k  = find k' l
 | k' > k  = find k' r

-- | member
--
member :: Ord k => k -> BST k v -> Bool
member _ Empty = False
member k' (Node k v l r)
 | k' == k = True
 | k' < k  = member k' l
 | k' > k  = member k' r

-- | union
--
union :: (Ord k) => BST k v -> BST k v -> BST k v
union m n = foldl' (\acc (k,v) -> insert k v acc) m $ toList n

-- | intersection
--
intersection :: (Ord k) => BST k v -> BST k v -> BST k v
intersection m n = foldl' (\acc (k,v) -> inter k v m acc) Empty $ toList n
 where
  inter k v t1 t2 =
   if (member k t1)
    then insert k v t2
    else t2

-- | difference
--
difference :: (Ord k) => BST k v -> BST k v -> BST k v
difference m n = foldl' (\acc (k, v) -> diff k v m acc) Empty $ toList n
 where
  diff k v t1 t2 =
   if (member k t1)
    then t2
    else insert k v t2

--join :: Ord k => BST k v -> BST k v -> BST k v
{-
join Empty n = n
join m Empty = m
join m n@(Node k' v' l' r') = update k' v' m
-}

-- | pp
--
pp :: (Show k, Show v) => BST k v -> IO ()
pp = (mapM_ putStrLn) . mapIndent
 where
  mapIndent Empty          = ["-- /-"]
  mapIndent (Node k v lb rb) =
   ["--(" ++ (show k) ++ "," ++ (show v) ++ ")"] ++
   map ("  |" ++) ls ++
   ("  `" ++ r) : map ("   " ++) rs
    where
     (r:rs) = mapIndent $ rb
     ls     = mapIndent $ lb

-- | solveChallenge
--
-- >>> solveChallenge 2 [4,5,2,5,4,3,1,3,4]
-- Just [4,5,3]
--
-- >>> solveChallenge 4 [4,5,2,5,4,3,1,3,4]
-- Nothing
--
-- >>> solveChallenge 2 [5,4,3,2,1,1,2,3,4,5]
-- Just [5,4,3,2,1]
--
solveChallenge :: Int -> [Int] -> Maybe [Int]
solveChallenge dups numbers =
  if null elements
     then Nothing
     else Just elements
  where
    elements =
      map fst $                                                -- we only want the first element in the tuple, our "a's"
        sortBy (\(a1,(b1,c1)) (a2,(b2,c2)) -> compare c1 c2) $ -- sortBy: sort by "c1 and c2", which is the order we first consumed these elements
          filter (\(a,(b,c)) -> b >= dups) $                   -- filter: we only want the elements that appear more than our provided dup value
            toList $                                           -- toList: converted our condensed BST back into a list
              fromListWith adder proper_tree_input             -- fromListWith:
                                                               -- create our BST with k/v pairs, accumulating on "b" and leaving "c" the same
                                                               -- b represents the accumulated dup value
                                                               -- c represents the initial index that this element was first consumed
    adder k (old,acc) (new,_) = (old + new, acc)
    proper_tree_input = zipWith3 (\a b c -> (a, (b,c))) numbers (repeat 1) [1..]

-- | solveChallengeIO
--
-- reads 2 lines:
-- reads 1 line: <numbers> <duplicates>
-- reads 1 line: <numbers ..>
--
solveChallengeIO :: IO ()
solveChallengeIO = do
  [num_numbers, duplicates] :: [Int] <- fmap (map read . words) getLine
  numbers :: [Int] <- fmap (map read . words) getLine
  let
    result  = solveChallenge duplicates numbers
    resultS = unwords $ map show $ (fromJust result)
  if result == Nothing
     then putStrLn "-1"
     else putStrLn resultS
  return ()

main :: IO ()
main = do
  test_cases <- readLn :: IO Int
  mapM_ (const $ solveChallengeIO) [1..test_cases]
