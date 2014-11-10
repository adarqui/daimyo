module Daimyo.Lib.Expressions.BoolTree (
    BoolBinOp (..),
    BoolUnOp (..),
    BoolType (..),
    BoolExpr (..),
    BoolTree (..),
    evaluate
) where

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
   | UExpr BoolUnOp BoolExpr deriving (Show)

data BElem = Op BoolBinOp | Bool BoolType deriving (Show)

data BoolTree = Empty | Node BoolTree BElem BoolTree deriving (Show)

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

t1 = BExpr (BVal T) And (BVal F)
t2 = BExpr (BVal T) Or (BVal F)
t3 = UExpr Not (BVal F)
t4 = BExpr (BExpr (BExpr (BVal T) Or (BVal F)) And (UExpr Not (BVal F))) Or (BExpr (BVal T) Or (BVal F))
