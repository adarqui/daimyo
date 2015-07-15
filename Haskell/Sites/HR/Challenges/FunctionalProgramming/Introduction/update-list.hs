{-# OPTIONS -O2 #-}

import           Prelude hiding (lines, map, mapM_)

data List a = Empty | Node a (List a) deriving (Show)

append :: a -> List a -> List a
append a Empty = Node a Empty
append a (Node a' r') = Node a' (append a r')

toList :: List a -> [a]
toList Empty = []
toList (Node a r) = a : toList r

lines :: String -> List String
lines = lines' Empty

lines' :: List Char -> String -> List String
lines' acc [] = Node (toList acc) Empty
lines' acc (c:cs)
 | c == '\n' = Node (toList acc) (lines' Empty cs)
 | c /= '\n' = lines' (c `append` acc) cs

map :: (a -> b) -> List a -> List b
map f Empty = Empty
map f (Node a r) = Node (f a) (map f r)

mapM_ :: (a -> IO ()) -> List a -> IO ()
mapM_ f Empty = return ()
mapM_ f (Node a r) = f a >> mapM_ f r

main :: IO ()
main = do
 v <- getContents >>= \contents -> return $ map (\line -> read line :: Int) $ lines contents
 mapM_ (\e -> putStrLn $ show $ abs e) v
