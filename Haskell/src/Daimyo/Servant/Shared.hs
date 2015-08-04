module Daimyo.Servant.Shared (
  Store,
  newBigState
) where

--
-- This file will contain a ton of state.. since all of our example apis will be loaded
--

import Control.Concurrent.STM
import Daimyo.Application.Todo.Simple

data BigState = BigState {
  appTodoSimple :: TodoApp
}

type Store = TVar BigState

-- | newBigState
--
-- just creates our monstrous big state record
--
newBigState :: STM (TVar BigState)
newBigState =
  newTVar $ BigState {
  appTodoSimple = newTodoApp
}
