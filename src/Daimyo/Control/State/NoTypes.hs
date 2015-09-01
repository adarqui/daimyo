module Daimyo.Control.State.NoTypes (
  runState,
  evalState,
  execState,
  get,
  gets,
  put,
  modify
) where

import           Control.Applicative ()
import           Control.Monad

-- | runState
--
runState :: (s -> (a, s)) -> s -> (a, s)
runState m s = m s

-- | evalState
--
evalState :: (s -> (a, s)) -> s -> a
evalState m s = fst $ runState m s

-- | execState
--
execState :: (s -> (a, s)) -> s -> s
execState m s = snd $ runState m s

-- | get
--
get :: (s -> (s, s))
get = \s -> (s, s)

-- | gets
--
-- get specific component of the state using a projection function
--
gets :: (s -> a) -> (s -> ((), s))
gets f = \s -> ((), s)

-- | put
--
put :: s -> (s -> ((), s))
put s = \_ -> ((), s)

-- | modify
--
modify :: (s -> s) -> (s -> ((), s))
modify f = \s' -> ((), f s')

-- | Examples
--
-- since we're just using functions, we're already instances of everything.
--
-- >>> runState (modify (+1) >> modify (+1)) (5 :: Int)
-- ((),7)
--
-- >>> runState (modify (+1) >> modify (+1) >> get) (5 :: Int)
-- (7,7)
--
-- >>> let swap (a,b) = (b,a) in runState (modify swap >> modify swap) (True, False)
-- ((),(False,True))
--
