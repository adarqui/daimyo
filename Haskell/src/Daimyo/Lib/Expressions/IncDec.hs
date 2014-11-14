module Daimyo.Lib.Expressions.IncDec (
) where

import Data.Char

data Op = Inc | Dec | IncBy | DecBy deriving (Show)

data Exp =
      Exp Op Exp
    | Val Int
    deriving (Show)

data Token =
      TokOp Op
    | TokVal Int
    | TokLParen
    | TokRParen
    | TokEnd
    deriving (Show)

tokOp s
    | s == 'i' = Inc
    | s == 'd' = Dec
    | s == '+' = IncBy
    | s == '-' = DecBy

tokenize = tokenizeS . map toLower

tokenizeS [] = [TokEnd]
tokenizeS (s:ss)
    | elem s ['i','d'] = TokOp (tokOp s) : tokenizeS ss
    | elem s ['+','-'] = TokOp (tokOp s) : TokVal (read $ takeWhile isDigit ss) : tokenizeS (dropWhile isDigit ss)
    | s == '(' = TokLParen : tokenizeS ss
    | s == ')' = TokRParen : tokenizeS ss

-- State evaluator

evaluateL n [] = 0
evaluateL n (x:xs) = 0


t1 = (Exp Inc (Exp Dec (Exp IncBy (Val 1))))
t2 = tokenize "iiidd"
t3 = tokenize "iii+97d"
