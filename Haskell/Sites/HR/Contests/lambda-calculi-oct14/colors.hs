{-# OPTIONS -O2 #-}
import Data.List

main :: IO ()
main = do
 _T <- readLn :: IO Int
 case (1 <= _T && _T <= 10) of
  True -> do
   colors <- mapM (\_ -> getLine) [1.._T]
   mapM_ (\rgyb -> putStrLn $ show $ calc rgyb) colors
  _ -> error "Constraint error: 1 <= T <= 10"

calc s =
 let v = calc' 0 0 0 0 s in
  case v of
   (Just (r,g,y,b)) -> (r == g && y == b)
   Nothing -> False

-- accumulator
calc' r g y b [] = Just (r,g,y,b)
calc' r g y b (s:ss)
 | s == 'R' = calc'rg (r+1) g y b ss
 | s == 'G' = calc'rg r (g+1) y b ss
 | s == 'Y' = calc'yb r g (y+1) b ss
 | s == 'B' = calc'yb r g y (b+1) ss
 | otherwise = error "Constraint error: Value other than RGYB provided"

calc'rg r g y b s
 | r - g <= 1 = calc' r g y b s
 | otherwise = Nothing

calc'yb r g y b s
 | y - b <= 1 = calc' r g y b s
 | otherwise = Nothing
