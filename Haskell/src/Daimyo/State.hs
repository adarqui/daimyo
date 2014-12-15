module Daimyo.State (
    State (..),
    evalState,
    execState,
    get,
    put,
    modify
) where

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

execState :: State s a -> s -> s
execState m s = snd $ runState m s

get = State $ \s -> (s, s)
put s = State $ \_ -> ((), s)
modify f s = State $ \s' -> ((), f s')

instance Functor (State s) where
    fmap f m = State $ \s ->
        let
            (a, s') = runState m s
        in
            (f a, s')

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k = State $ \s ->
        let
            (a, s') = runState m s
        in
            runState (k a) s'

t = (put 6 >> modify (+1) get >> get)
t1 = evalState t (5 :: Int)
t2 = execState t (5 :: Int)
t3 = runState t (5:: Int)
t4 = runState (put 6 >> get) (5 :: Int)
t5 = runState (State $ \s -> (1,s)) 2
t6 = runState (let st = (State $ \s -> (1,s)) in st) 2
t7 = runState (let st = (State $ \s -> (1,s)) in st) 2
