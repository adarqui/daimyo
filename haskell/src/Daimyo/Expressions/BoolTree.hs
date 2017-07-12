module Daimyo.Expressions.BoolTree (
    BoolBinOp (..),
    BoolUnOp (..),
    BoolType (..),
    BoolExpr (..),
    BoolTree (..),
    evaluate
) where

import Data.Char

{-
sources:
http://en.wikipedia.org/wiki/Truth_table
http://en.wikipedia.org/wiki/Propositional_calculus
http://www.iep.utm.edu/prop-log/
-}

data BoolBinOp = And | Or | Nand | Nor | XNor | Xor deriving (Show)
data BoolUnOp = Not deriving (Show)
data BoolType = T | F deriving (Show)
data BoolExpr =
     BVal BoolType
   | BExpr BoolExpr BoolBinOp BoolExpr
   | UExpr BoolUnOp BoolExpr
    deriving (Show)

data BElem = Op BoolBinOp | Bool BoolType deriving (Show)

data BoolTree = Empty | Node BoolTree BElem BoolTree deriving (Show)

data ETree =
      BinNode BoolBinOp ETree ETree
    | UnNode BoolUnOp ETree
    | BoolNode BoolType
--    | VarNode String
    deriving (Show)

data Token =
      TokOp BoolBinOp
    | TokIdent String
    | TokBool BoolType
    | TokLParen
    | TokRParen
    | TokEnd
    deriving (Show)

update b Empty = Node Empty b Empty
update b (Node lb b' rb) = Empty

evaluate :: BoolExpr -> BoolType
evaluate (BVal t) = t
evaluate (BExpr exp1 bop exp2) = bexp (evaluate exp1) bop (evaluate exp2)
evaluate (UExpr bop exp1) = uexp bop (evaluate exp1)

bexp T And T = T
bexp _ And _ = F

bexp F Or F = F
bexp _ Or _ = T

bexp T Nand T = F
bexp _ Nand _ = T

bexp T Xor F = T
bexp F Xor T = T
bexp _ Xor _ = F

bexp F Nor F = T
bexp _ Nor _ = F

bexp T XNor T = T
bexp F XNor F = T
bexp _ XNor _ = F

uexp Not T = F
uexp Not F = T

tokOp :: String -> BoolBinOp
tokOp s
    | s == "and" = And
    | s == "or" = Or
    | s == "nand" = Nand
    | s == "nor" = Nor
    | s == "xnor" = XNor
    | s == "xor" = Xor

tokBool :: String -> BoolType
tokBool s
    | s == "t" = T
    | s == "f" = F

tokenize = tokenizeS . words . map toLower

tokenizeS [] = [TokEnd]
tokenizeS (s:ss)
    | elem s ["and","or","nand","nor","xnor","xor"] = TokOp (tokOp s) : tokenizeS ss
    | elem s ["t","f"] = TokBool (tokBool s) : tokenizeS ss
    | s == "(" = TokLParen : tokenizeS ss
    | s == ")" = TokRParen : tokenizeS ss
    | otherwise = error $ "invalid token: " ++ s

t1 = BExpr (BVal T) And (BVal F)
t2 = BExpr (BVal T) Or (BVal F)
t3 = UExpr Not (BVal F)
t4 = BExpr (BExpr (BExpr (BVal T) Or (BVal F)) And (UExpr Not (BVal F))) Or (BExpr (BVal T) Or (BVal F))
t5 = tokenize "t and t or f"
t6 = tokenize "t and ( t or f )"
