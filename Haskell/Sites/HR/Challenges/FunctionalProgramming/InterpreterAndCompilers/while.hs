{- GRAMMAR:
x, y ∈ Var (variables)
n ∈ Num (numerals/integers)

opa ∈ Opa (arithmetic operators) 
oba ::= + | - | * | /

opb ∈ Opb (boolean operators) 
opb ::= and | or

opr ∈ Opr (relational operators) 
opr ::= > | <

a ∈ AExp (arithmetic expressions) 
a ::= x | n | a1 opa a2 | ( a )

b ∈ BExp (boolean expressions) 
b ::= true | false | b1 opb b2 | a1 opr a2 | ( b )

S ∈ Stmt (statements) 
S ::= x := a | S1 ; S2 | if b then { S1 } else { S2 } | while b do { S }
-}

{- PRECEDENCE:
Arithmetic Operators: (*, /) > (+, -) > (>, <)
Boolean Operators: and > or
-}

{- EXAMPLE:
fact := 1 ;
val := 10000 ;
cur := val ;
mod := 1000000007 ;

while ( cur > 1 )
  do
   {
      fact := fact * cur ;
      fact := fact - fact / mod * mod ;
      cur := cur - 1
   } ;

cur := 0
-}

data Tree k v = Empty | Node (Tree k v) (k,v) (Tree k v) deriving (Show)

data NumBinOp = Add | Sub | Mul | Div deriving (Show)
data BoolBinOp = And | Or deriving (Show)
data OrdBinOp = GT | LT deriving (Show)

data BExpr =
      BBinary NumBinOp BExpr BExpr
      deriving (Show)
--    | OBinary OrdBinOp 

data AExpr =
      Var String
    | Const Integer
    | ABinary NumBinOp AExpr AExpr
    | Assign String AExpr
      deriving (Show)

data Stmt =
      If BExpr Stmt Stmt
    | While BExpr Stmt
      deriving (Show)

data State = State {
    vars :: Tree String Integer
} deriving (Show)


update k v Empty = Node Empty (k,v) Empty
update k v (Node lb (k',v') rb) = Empty

main :: IO ()
main = do
    print "while"

t1 = [Assign "fact" (Const 1), Assign "val" (Const 10000), Assign "cur" (Var "val"), Assign "mod" (Const 1000000007)]
