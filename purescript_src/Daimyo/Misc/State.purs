module Daimyo.Misc.State where

import Prelude
import Data.JSON
import Data.Maybe
import Data.Tuple
import Control.Monad
import Control.Monad.Eff
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import qualified Data.Map as M

import Daimyo.Control.Monad

-- Simple Counter
--

type Counter = Int
type CounterState a = State Counter a

incr :: forall eff a. CounterState Counter
incr = do
  modify (+1)
  get

decr :: forall eff a. CounterState Counter
decr = do
  modify (\c -> c - 1)
  get

getr :: forall eff a. CounterState Counter
getr = get

runCounter :: Tuple Counter Counter
runCounter = runState (incr >> incr >> incr >> decr >> decr) 0


-- Simple Counter: stored in a record
--

data CRec = CRec {
  counter :: Counter
}

type CRecState a = State CRec a

crecGetCounter :: CRec -> Counter
crecGetCounter (CRec{counter:c}) = c

crecPlus1 :: CRec -> CRec
crecPlus1 (CRec{counter:c}) = CRec { counter: c+1 }

crecMinus1 :: CRec -> CRec
crecMinus1 (CRec{counter:c}) = CRec { counter: c-1 }

crecGet :: forall eff a. CRecState Counter
crecGet = gets crecGetCounter

crecIncr :: forall eff a. CRecState Counter
crecIncr = modify crecPlus1 >> gets crecGetCounter

crecDecr :: forall eff a. CRecState Counter
crecDecr = modify crecMinus1 >> gets crecGetCounter

runCRec :: Counter
runCRec = evalState (crecIncr >> crecIncr >> crecIncr >> crecDecr >> crecDecr) (CRec{counter:0})
