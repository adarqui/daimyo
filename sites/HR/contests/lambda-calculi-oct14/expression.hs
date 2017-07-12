{-# OPTIONS -O2 #-}
import Data.List

data NumOp = Number Integer | Add | Sub | Mul deriving (Show)

main :: IO ()
main = do
 _N <- readLn :: IO Integer
 _L <- getLine
 let numbers = build'List _L
 case (2 <= _N && _N <= (10^4) && all (\n -> n >= 1 && n <= 100) numbers) of
  True -> putStrLn $ concat $ prettify $ snd $ find'101 numbers
  _ -> error "Constraint error: 2 <= N <= 10^4 || 1 <= element <= 100"

build'List [] = []
build'List s = read ("[" ++ (map (\n -> if n == ' ' then ',' else n) s) ++ "]") :: [Integer]

find'101 nums = find'101' $ gen'numops nums

find'101' genned@(g:gs) =
 if (result `rem` 101 == 0) then (result, history) else find'101' gs
 where
  (result, history) = apply g


prettify [] = []
prettify (numop:numops) = (numop'to'String numop) : prettify numops

gen'ops n = permutations $ take n $ cycle [Add,Sub,Mul]
gen'numops nums = map (\ops -> merge (map Number nums) (take (length nums - 1) ops)) $ (gen'ops $ (length nums))

apply numops =
 (fst $ foldl' (\(num,op) numop ->
  case numop of
   (Number n) -> (op num n,op)
   _ -> (num, numop'translate numop)
  )
 (0,(+))
 numops, numops)

fromNumber (Number n) = n

numop'translate numop = case numop of
 Add -> (+)
 Sub -> (-)
 Mul -> (*)

numop'to'String numop =
 case numop of
  Number n -> show n
  Add -> "+"
  Sub -> "-"
  Mul -> "*"

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs
