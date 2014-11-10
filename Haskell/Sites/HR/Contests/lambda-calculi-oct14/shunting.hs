import Data.Char
 
prec :: String -> Double
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
 
leftAssoc _ = False
 
isOp (t:[]) = t `elem` "-+/*^"
isOp _      = False

isOp' '+' = True
isOp' '-' = True
isOp' '*' = True
isOp' '/' = True
isOp' _ = False

isParen '(' = True
isParen ')' = True
isParen _ = False

isntOp = not . isOp
 
shunting'Mutant xs = final ++ [lastStep]
  where final = scanl f ([],[],"") xs
        lastStep = (\(x,y,_) -> (reverse y ++ x, [], "")) $ last final
        f (out,st,_) t | isOp t =
                         (reverse (takeWhile testOp st) ++ out
                         , (t:) $ (dropWhile testOp st), t)
                       | t == "(" = (out, "(":st, t)
                       | t == ")" = (reverse (takeWhile (/="(") st) ++ out,
                                     tail $ dropWhile (/="(") st, t)
                       | True     = (t:out, st, t)
          where testOp x = isOp x && (prec t < prec x)
 
main = do
    a <- getLine
    let (x,y,z) = last $ shunting'Mutant $ parse a
    putStrLn $ show $ reverse x
    putStrLn $ show $ eval $ reverse x

parse s = parse' s

parse' :: [Char] -> [[Char]]
parse' s = filter (/= "") $ parse'' False [] [] $ filter (not . isSpace) s

parse'' :: Bool -> [Char] -> [[Char]] -> [Char] -> [[Char]]
parse'' _ op accum [] = accum ++ [op]
parse'' prevOp op accum (s:ss)
 | isParen s = parse'' False [] (accum ++ [op] ++ [[s]]) ss
 | prevOp == False && isOp' s = parse'' True [] (accum ++ [op] ++ [[s]]) ss
 | prevOp == True && isOp' s = parse'' True (op ++ [s]) accum ss
 | otherwise = parse'' False (op ++ [s]) accum ss


eval'modp s = (fromIntegral $ round $ eval s) `mod` modp

eval :: [String] -> Double
eval s = eval' 0 [] s

eval' acc stack [] = head stack
eval' acc stack (n@(x:r):xs) =
 case (isntOp n) of
  True -> eval' acc ((read n :: Double) : stack) xs
  False -> eval' acc (binres x : binrest) xs
 where
  (biny,binx,binrest) = (stack !! 0, stack !! 1, drop 2 stack)
  binres op = apply'Op op binx biny
  (unx,unrest) = (stack !! 0, tail stack)

apply'Op :: Char -> Double -> Double -> Double
apply'Op '+' x y = x + y
apply'Op '-' x y = x - y
apply'Op '*' x y = x * y
apply'Op '/' x y = x / y


t1 = eval'modp $ bang "22 * 79 - 21"
t2 = eval'modp $ bang "4 / -2 / 2 + 8"
t3 = eval'modp $ bang "55 + 3 - 45 * 33 - 25"
t4 = eval'modp $ bang "4 / -2 / ( 2 + 8 )"
t5 = eval'modp $ bang "22*79-21"
t6 = eval'modp $ bang "4/-2/2 + 8"
t7 = eval'modp $ bang "55+3-45*33-25"
t8 = eval'modp $ bang "4/-2/(2+8)"


modp = (10^9) + 7

bang s = let (x,_,_) = (last $ shunting'Mutant $ parse s) in reverse x

{-
http://www.reedbeta.com/blog/2011/12/11/the-shunting-yard-algorithm/
operator, any operators of equal precedence at the top of the stack should be popped and applied. This makes those operators left-associative, since the leftmost of the two operators will be applied first. You can implement right-associativity by leaving equal-precedence operators on the stack.

http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
-}
