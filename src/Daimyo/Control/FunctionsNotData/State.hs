module Daimyo.Control.FunctionsNotData.State (
  State,
  runState,
  evalState,
  execState,
  returnState,
  composeState,
  composeState_,
  get,
  put,
  modify
) where

-- | State
--
type State s a = s -> (a, s)

-- | runState
--
runState :: State s a -> (s -> (a, s))
runState m = \s' -> m s'

-- | evalState
--
evalState :: State s a -> s -> a
evalState = (fst .) . runState

-- | execState
--
execState :: State s a -> s -> s
execState = (snd .) . runState

-- | returnState
--
-- similar to return
--
returnState :: a -> State s a
returnState a = \s -> (a, s)

-- | composeState
--
-- similar to >>=
--
composeState :: State s t -> (t -> State s a) -> State s a
composeState m k = \s -> let (a, s') = runState m s in runState (k a) s'

-- | composeState_
--
-- similar to >>
--
composeState_ :: State s a -> State s a -> State s a
composeState_ m k = \s -> let (a, s') = runState m s in runState k s'

-- | get
--
get :: State s s
get = \s -> (s, s)

-- | put
--
put :: s -> State s ()
put s = \_ -> ((), s)

-- | modify
--
modify :: (s -> s) -> State s ()
modify f = \s -> ((), f s)

-- Examples
--
-- >>> runState (put 5) 0 :: ((), Int)
-- ((),5)
--
-- >>> execState (put 5) 1 :: Int
-- 5
--
-- >>> evalState (put 5) (1 :: Int) :: ()
-- ()
--
-- >>> evalState (returnState "hi") True
-- "hi"
--
-- >>> execState (modify (\s -> s ++ "!")) "hi"
-- "hi!"
--
-- >>> execState (\s -> let s' = execState get s in runState (modify (++ "!")) s') "hi"
-- "hi!"
--
-- >>> execState (modify (++ "!") `composeState` \_ -> modify (++ "!")) "hi"
-- "hi!!"
--
-- >>> execState (modify (++ "!") `composeState_` modify (++ "!") `composeState_` modify (++ "!")) "hi"
-- "hi!!!"
--
-- >>> runState (returnState (1 :: Int) `composeState_` returnState 2 `composeState_` returnState 3) "hi"
-- (3,"hi")
