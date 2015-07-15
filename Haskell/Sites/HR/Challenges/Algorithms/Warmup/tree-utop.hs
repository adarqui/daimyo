import           Control.Monad
import           Data.Maybe

main :: IO ()
main = do
 t <- readLn
 case (1 <= t && t <= 10) of
  True -> do
   m <- mapM (\x -> readLn) [1..t]
   mapM_ (putStrLn . show) $ catMaybes $ map utopia m
  _ -> error "Constraint Error: 1 <= T <= 10"

utopia n =
 case (0 <= n && n <= 60) of
  True -> Just $ foldl (\x y -> y x) 0 $ (take (n+1) $ cycle [(+)1,(*)2])
  _ -> Nothing
