import           Data.Bits
import           Data.List
import           GHC.Integer
import           Prelude     hiding (mapM_)

data RoundTree a = Empty | Node (RoundTree a) a (RoundTree a) deriving (Show)

mapM_ f [] = return ()
mapM_ f (x:xs) = f x >> mapM_ f xs

replicateM 0 f = return []
replicateM n f = do
    r1 <- f
    r2 <- replicateM (n-1) f
    return $ r1 : r2

fromList :: (Ord a) => [a] -> Int -> RoundTree (a, Integer)
fromList ks round = fromList' ks round Empty

fromList' :: (Ord a) => [a] -> Int -> RoundTree (a, Integer) -> RoundTree (a, Integer)
fromList' [] _ rt = rt
fromList' (k:ks) round rt = update k round (fromList' ks round rt)

update :: (Ord a) => a -> Int -> RoundTree (a, Integer) -> RoundTree (a, Integer)
update k round Empty = Node Empty (k, setBit 0 round) Empty
update k round (Node lb (k', round') rb)
    | k == k' = Node lb (k', setBit round' round) rb
    | k < k' = Node (update k round lb) (k',round') rb
    | k > k' = Node lb (k',round') (update k round rb)

filterN f Empty = []
filterN f (Node lb (x,n) rb) =
    case (f n) of
        True -> x : filterN f lb ++ filterN f rb
        False -> filterN f lb ++ filterN f rb

calcBits :: Integer -> Integer
calcBits n = foldl' (\acc i -> setBit acc (fromIntegral i)) 0 [1..n]

solve n strings = length $ filterN (== (calcBits $ fromIntegral n)) $ foldl' (\acc (s,i) -> fromList' s i acc) Empty $ zip strings [1..n]

main :: IO ()
main = do
    _T <- readLn :: IO Integer
    lines' <- replicateM _T getLine
    putStrLn $ show $ solve (fromIntegral _T) lines'

t1 = fromList' "eeabg" 3 $ fromList' "baccd" 2 $ fromList "abcdde" 1
t2 = filterN (== 14) t1
