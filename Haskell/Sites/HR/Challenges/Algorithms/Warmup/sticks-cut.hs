import Data.List
import Prelude hiding (mapM_)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

mapM_ f [] = return ()
mapM_ f (x:xs) = f x >> mapM_ f xs

fromList [] = Empty
fromList (x:xs) = update x (fromList xs)

update k Empty = Node Empty (k,1) Empty
update k (Node lb (x,y) rb)
    | k == x = Node lb (x,y+1) rb
    | k < x = Node (update k lb) (x,y) rb
    | k > x = Node lb (x,y) (update k rb)

inOrder Empty = []
inOrder (Node lb (x,n) rb) = (inOrder lb) ++ (replicate n x) ++ (inOrder rb)

main :: IO ()
main = do
    t <- readLn :: IO Int
    ns <- (getLine >>= \line -> return $ map read $ words line)
    mapM_ (\ans -> putStrLn $ show ans) $ solve ns

solve = solve' . inOrder . fromList
solve' [] = []
solve' (n:ns) =
    (1+length ns) : (solve' $ map (`minus` n) remaining)
    where
        remaining = dropWhile (==n) ns

minus = (-)
