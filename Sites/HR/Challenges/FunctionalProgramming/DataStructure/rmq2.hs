{-# LANGUAGE NamedFieldPuns #-}

import           Data.Foldable hiding (elem)
import           Data.Maybe
import           Data.Monoid
import           Prelude       hiding (elem, foldl, foldr, length, map, null)
import qualified Prelude       as P

data Tree a = Nil
    | Leaf {
        value :: a
    }
    | Junction {
        value :: a,
        nodes :: Int,
        left  :: Tree a,
        right :: Tree a
    } deriving (Show, Read, Eq)

instance Foldable Tree where
    foldr f = go
        where
            go a Nil = a
            go a Leaf{value} = f value a
            go a Junction{value, left, right} = go (go a right) left

instance Monoid a => Monoid (Tree a) where
    mempty = Nil

    Nil `mappend` x = x
    x `mappend` Nil = x

    l@Leaf{value = v1} `mappend` r@Leaf{value = v2} =
        Junction{
            value = v1 <> v2,
            nodes = 2,
            left = l,
            right = r
        }

    l@Junction{value = v1, nodes = n1} `mappend` r@Junction{value = v2, nodes = n2} =
        Junction{
            value = v1 <> v2,
            nodes = n1 + n2,
            left = l,
            right = r
        }

    l@Leaf{value = v1} `mappend` r@Junction{value = v2, nodes = n} =
        Junction{
            value = v1 <> v2,
            nodes = n + 1,
            left = l,
            right = r
        }

    l@Junction{value = v1, nodes = n} `mappend` r@Leaf{value = v2} =
        Junction{
            value = v1 <> v2,
            nodes = n + 1,
            left = l,
            right = r
        }

empty :: Tree a
empty = Nil

singleton :: (Monoid a) => a -> Tree a
singleton = Leaf

length :: (Num i) => Tree a -> i
length Nil = 0
length Leaf{} = 1
length Junction{nodes} = fromIntegral nodes

rmq :: (Monoid a) => Tree a -> (Int, Int) -> Maybe a
rmq Nil _ = Nothing
rmq Leaf{value} _ = Just value
rmq Junction{value, nodes, left, right} (b, e)
    | b < 0 || e > nodes || b >= e = error "Data.Tree.Segment.rmq: indexes out of bounds"
    | b == 0 && e == nodes = Just value
    | e <= m = rmq left (b, e)
    | b >= m = rmq right (b - m, e - m)
    | otherwise = rmq left (b, m) <> rmq right (0, e - m)
    where
        m = length left

makeTree ints = mconcat $ foldl' (\acc n -> acc ++ [singleton [n]]) [] ints

main = do
 _ <- getLine
 nums <- getLine >>= \contents -> return $ P.map (\n -> read n :: Int) $ words contents
 queries <- getContents >>= \contents -> return $ P.map (\line -> let (x:y:[]) = words line in (read x::Int,read y::Int)) $ lines contents
 let tree = makeTree nums
 P.mapM_ (\(qx,qy) -> putStrLn $ show $ rmq tree (qx,qy+1)) queries

t1 = makeTree [10,20,30,40,11,22,33,44,15,5]
t2 = makeTree [2,5,1,4,9,3]
