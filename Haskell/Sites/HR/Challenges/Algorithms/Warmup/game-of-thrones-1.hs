import           Data.List
import           Prelude   hiding (mapM_)

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

filterN f Empty = []
filterN f (Node lb (x,n) rb) =
    case (f n) of
        True -> x : filterN f lb ++ filterN f rb
        False -> filterN f lb ++ filterN f rb

main :: IO ()
main = do
    string <- getLine
    putStrLn $ solve string

solve string =
    case (odds <= 1) of
        True -> "YES"
        False -> "NO"
    where
        odds = length $ filterN odd $ fromList string
