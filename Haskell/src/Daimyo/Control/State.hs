module Daimyo.Control.State (
  State (..),
  evalState,
  execState,
  get,
  put,
  modify
) where

import           Control.Applicative
import           Control.Monad

-- | State
--
newtype State s a = State { runState :: s -> (a, s) }

-- | evalState
--
evalState :: State s a -> s -> a
evalState m s = fst $ runState m s

-- | execState
--
execState :: State s a -> s -> s
execState m s = snd $ runState m s

-- | get
--
get :: State a a
get = State $ \s -> (s, s)

-- | put
--
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- | modify
--
modify :: (s -> s) ->  State s ()
modify f = State $ \s' -> ((), f s')

instance Functor (State s) where
  fmap f m = State $ \s -> let (a, s') = runState m s in (f a, s')

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k  = State $ \s -> let (a, s') = runState m s in runState (k a) s'

-- | Examples
--
-- >>> runState (modify (+1) >> modify (+1)) (5 :: Int)
-- ((),7)
--
-- >>> runState (modify (+1) >> modify (+1) >> get) (5 :: Int)
-- (7,7)
--
