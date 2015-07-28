import           Prelude hiding (mapM_)

data Trip = Trip (Int,Int,Int) deriving (Show)

new n c m = Trip (n,c,m)

mapM_ f [] = return ()
mapM_ f (x:xs) = do
    f x
    mapM_ f xs

getTrips :: Int -> IO [Trip]
getTrips 0 = return []
getTrips n = do
    t1 <- getLine
    let w = words t1
    let t1' = Trip (read (w !! 0), read (w !! 1), read (w !! 2))
    t2 <- getTrips (n-1)
    return $ t1' : t2

solve (Trip (n,c,m)) = chocolates + solve' chocolates c m
    where
        chocolates = n `div` c

solve' chocolates c m
    | chocolates < m = 0
    | otherwise = newChocolates + solve' (newChocolates + leftovers) c m
    where
        (newChocolates, leftovers) = (chocolates `div` m, chocolates `rem` m)

main :: IO ()
main = do
    t <- readLn :: IO Int
    trips <- getTrips t
    mapM_ (putStrLn . show . solve) trips
