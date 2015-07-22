module Daimyo.Algebra.Expression (
  Degree,
  Coeff,
  Var (..),
  Value (..),
  Expr (..)
) where

type Degree = Int
type Coeff  = Int

data Var
  = X
  | Y
  | Z
  deriving (Show)

data Value a
  = Const a
  | Frac a a
  deriving (Show)

data Expr
  = ConstTerm Double Degree
  | VarTerm Coeff Var Degree
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sqrt Expr
  | Exp Expr Int deriving (Show)

-- simplify :: Expr -> Expr
-- eval :: Expr ->

t_expr1 = Add (ConstTerm 5 1) (ConstTerm 2 2)
t_expr2 = Add (VarTerm 1 X 2) (VarTerm 3 X 4)
