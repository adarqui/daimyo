module Daimyo.Expressions.ShuntingYard (
) where

-- made this too complex for now.. it's getting nutty

-- http://andreinc.net/2010/10/05/converting-infix-to-rpn-shunting-yard-algorithm/

import Data.List

data Fixity = LeftF | RightF | NoneF deriving (Show)

data Operator a = Operator {
    tok :: String,
    bag :: a,
    fix :: Fixity,
    prec :: Int
} deriving (Show)

data Tree k v = TEmpty | TNode (Tree k v) (k, v) (Tree k v) deriving (Show)

data Stack a = StkEmpty | StkNode a (Stack a) deriving (Show)

data Queue a = QEmpty | QNode a (Queue a) deriving (Show)

newtype State s a = State { runState :: s -> (a,s) }

data ShuntEnv a b = ShuntEnv {
    tree :: Tree String (Operator b),
    stack :: Stack a,
    queue :: Queue a,
    opCnt :: Int
} deriving (Show)


-- tree stuff

tFromList [] = TEmpty
tFromList ((x,y):xs) = TNode (tFromList $ takeWhile (\(x',y') -> x' < x) xs) (x,y) (tFromList $ takeWhile (\(x',y') -> x' > x) xs)

tFind _ TEmpty = Nothing
tFind e (TNode lb (k,v) rb)
    | e == k = Just (k,v)
    | e < k = tFind e lb
    | e > k = tFind e rb

-- stack stuff

--stkFromList [] = StkEmpty
stkFromList = foldl' (\acc e -> StkNode e acc) StkEmpty

push e StkEmpty = StkNode e StkEmpty
push e r = StkNode e r

pop StkEmpty = StkEmpty
pop (StkNode e' rest) = rest

top StkEmpty = Nothing
top (StkNode e' rest) = Just e'

-- queue stuff

qFromList [] = QEmpty
qFromList (x:xs) = QNode x (qFromList xs)

enqueue e QEmpty = QNode e QEmpty
enqueue e (QNode e' rest) = QNode e' (enqueue e rest)

dequeue QEmpty = QEmpty
dequeue (QNode _ rest) = rest

bottom QEmpty = Nothing
bottom (QNode e' rest) = Just e'

-- state stuff

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k =
        State $ \s ->
            let
                (a, s') = runState m s
            in
                runState (k a) s'

evalState :: State s a -> s -> a
evalState m s  = fst $ runState m s

buildOp tok bag fix prec = Operator { tok = tok, bag = bag, fix = fix, prec = prec }

shunt [] = []
shunt (x:xs) = []


-- tests

data TestOp = Add | Sub | Mul | Div | Exp deriving (Show)
data TestTokens = Op TestOp | Val Int | Boo Bool deriving (Show)

t_ops = [buildOp "+" Add LeftF 1, buildOp "-" Sub LeftF 1, buildOp "*" Mul LeftF 2, buildOp "/" Div LeftF 2, buildOp "^" Exp RightF 3]
t_ops' = map (\rec -> (tok rec, rec)) t_ops
t_tree = tFromList t_ops'
t_find = tFind "+" t_tree

t_shunt_env = ShuntEnv {
    tree = t_tree,
    stack = StkEmpty,
    queue = QEmpty,
    opCnt = 0   
}

--t_shunt_eval t_shunt_env 


-- stack tests

t_stk_fromList = stkFromList [1..10]
t_stk_push = foldl' (\acc e -> push e acc) StkEmpty [1..10]
t_stk_pop = top $ foldl' (\acc _ -> pop acc) t_stk_fromList [1..3]

-- queue tests

t_q_fromList = qFromList [1..10]
t_q_enqueue = foldl' (\acc e -> enqueue e acc) QEmpty [1..10]
t_q_dequeue = bottom $ foldl' (\acc _ -> dequeue acc) t_q_enqueue [1..3]
