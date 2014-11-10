{-# OPTIONS -O2 #-}
import Data.Char
 
prec :: String -> Int
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
 
isOp (t:[]) = t `elem` "-+/*^"
isOp _      = False

isntOp = not . isOp

isOp' c = c == '+' || c == '-' || c == '*' || c == '/'
isParen c = c == '(' || c == ')'

shunting xs =
 final ++ [lastStep]
  where
   final = scanl f ([],[],"") xs
   lastStep = (\(x,y,_) -> (reverse y ++ x, [], "")) $ last final
   f (out,st,_) t
    | isOp t = (reverse (takeWhile testOp st) ++ out , (t:) $ (dropWhile testOp st), t)
    | t == "(" = (out, "(":st, t)
    | t == ")" = (reverse (takeWhile (/="(") st) ++ out, tail $ dropWhile (/="(") st, t)
    | True     = (t:out, st, t)
--    where testOp x = isOp x && (False && prec t == prec x || prec t < prec x)
    where testOp x = isOp x && (prec t < prec x)

shunting' expr = (let (x,_,_) = last $ shunting $ parse expr in reverse x)
 
main = do
 expr <- getLine
 case (length expr < (10^5))  of
  True -> do
   putStrLn $ show $ bang expr
  _ -> error "Constraint error: Length of expr < 10^5"

bang s = let (x,_,_) = (last $ shunting $ parse s) in eval'modp $ reverse x

parse :: [Char] -> [[Char]]
parse s = filter (/= "") $ parse' True [] [] $ filter (not . isSpace) s

parse' :: Bool -> [Char] -> [[Char]] -> [Char] -> [[Char]]
parse' _ op accum [] = accum ++ [op]
parse' prevOp op accum (s:ss)
 | isParen s = parse' prevOp [] (accum ++ [op] ++ [[s]]) ss
-- | prevOp == False && isOp' s = parse' True [] (accum ++ [op] ++ [[s]]) ss
 | prevOp == False && isOp' s = parse' True [] (accum ++ [op] ++ [[s]]) ss
 | prevOp == True && isOp' s = parse' True (op ++ [s]) accum ss
 | otherwise = parse' False (op ++ [s]) accum ss

eval'modp s = (eval s) `mod` modp

eval :: [String] -> Int
eval s = eval' 0 [] s

eval' acc stack [] = head stack
eval' acc stack (n@(x:r):xs) =
 case (isntOp n) of
  True -> case (n' < (10^9)) of
    True -> eval' acc (n' : stack) xs
    False -> error "Constraint error: n < 10^9"
   where
    n' = read n :: Int
  False -> eval' acc (binres x : binrest) xs
 where
  (biny,binx,binrest) = (stack !! 0, stack !! 1, drop 2 stack)
  binres op = apply'Op op binx biny
  (unx,unrest) = (stack !! 0, tail stack)

apply'Op :: Char -> Int -> Int -> Int
apply'Op '+' x y = x + y
apply'Op '-' x y = x - y
apply'Op '*' x y = x * y
apply'Op '/' x y = x `quot` y

modp = (10^9) + 7

eval'print acc stack [] = head stack
eval'print acc stack (n@(x:r):xs) =
 case (isntOp n) of
  True -> case (n' < (10^9)) of
    True -> eval'print acc (n' : stack) xs
    False -> error "Constraint error: n < 10^9"
   where
    n' = read n :: Int
  False -> eval'print acc (binres x : binrest) xs
 where
  (biny,binx,binrest) = (stack !! 0, stack !! 1, drop 2 stack)
  binres op = apply'Op op binx biny
  (unx,unrest) = (stack !! 0, tail stack)


eval'print'io acc stack [] = return $ head stack
eval'print'io acc stack rpn@(n@(x:r):xs) = do
 print $ "Stack: " ++ show stack ++ ", n: " ++ n ++ ", acc: " ++ show acc ++ ", expr: " ++ show rpn
 case (isOp n) of
  False -> do
    putStrLn "isnt Op.."
    case (n' < (10^9)) of
     True -> eval'print'io acc (n' : stack) xs
     False -> error "Constraint error: n < 10^9"
   where
    n' = readInt n
  True -> do
    putStrLn $ "is op.." ++ show binx ++ " " ++ n ++ " "  ++ show biny
    eval'print'io acc (binres x : binrest) xs
   where
    (biny,binx,binrest) = (stack !! 0, stack !! 1, drop 2 stack)
    binres op = apply'Op op binx biny
    (unx,unrest) = (stack !! 0, tail stack)

t1 = eval'print'io 0 [] $ shunting' "22 * 79 - 21"
t2 = eval'print'io 0 [] $ shunting' "4/-2/2 + 8"
t3 = eval'print'io 0 [] $ shunting' "55+3-45*33-25"
t4 = eval'print'io 0 [] $ shunting' "4/-2/(2 + 8)"
t5 = eval'print'io 0 [] $ shunting' "109-8*7+654*3-2/1"

readInt s = read (filter (/= '+') $ takeWhile (\c -> c == '+' || c == '-' || isDigit c) s) :: Int

parse'io :: [Char] -> IO [[Char]]
--parse'io s = filter (/= "") $ parse'io' True [] [] $ filter (not . isSpace) s
parse'io s = do
 r <- parse'io' True [] [] $ filter (not . isSpace) s
 return $ filter (/= "") r

parse'io' :: Bool -> [Char] -> [[Char]] -> [Char] -> IO [[Char]]
parse'io' _ op accum [] = do
 putStrLn $ "0: op: " ++ show op ++ ", accum: " ++ show accum
 return $ accum ++ [op]
parse'io' prevOp op accum (s:ss)
 | isParen s = do
   putStrLn $ "1: prevOp: " ++ show prevOp ++ ", op: " ++ show op ++ ", accum: " ++ show accum ++ ", s: " ++ show s
--   putStrLn $ "prevOp: " ++ show prevOp
   parse'io' prevOp [] (accum ++ [op] ++ [[s]]) ss
-- | prevOp == False && isOp' s = parse' True [] (accum ++ [op] ++ [[s]]) ss
 | prevOp == False && isOp' s = do
  putStrLn $ "2: prevOp: " ++ show prevOp ++ ", op: " ++ show op ++ ", accum: " ++ show accum ++ ", s: " ++ show s
  parse'io' True [] (accum ++ [op] ++ [[s]]) ss
 | prevOp == True && isOp' s = do
  putStrLn $ "3: prevOp: " ++ show prevOp ++ ", op: " ++ show op ++ ", accum: " ++ show accum ++ ", s: " ++ show s
  parse'io' True (op ++ [s]) accum ss
 | otherwise = do
  putStrLn $ "4: prevOp: " ++ show prevOp ++ ", op: " ++ show op ++ ", accum: " ++ show accum ++ ", s: " ++ show s
  parse'io' False (op ++ [s]) accum ss
