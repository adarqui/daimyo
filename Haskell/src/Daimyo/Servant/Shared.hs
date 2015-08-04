{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Daimyo.Servant.Shared (
  Store,
  LnAPI,
  daimyoAPI,
  newBigState
) where

--
-- This file will contain a ton of state.. since all of our example apis will be loaded
--

import           Control.Concurrent.STM
import           Daimyo.Application.Todo.Simple
import           Servant

data BigState = BigState {
  appTodoSimple :: TodoApp
}

type Store = TVar BigState

type LnAPI =
       "static"         :> Raw
  :<|> "ping"           :> Get '[JSON] String
  -- application: todo simple
  :<|> "applications/todo/simple/list" :> Get '[JSON] [Todo]

daimyoAPI :: Proxy LnAPI
daimyoAPI = Proxy

-- | newBigState
--
-- just creates our monstrous big state record
--
newBigState :: IO (TVar BigState)
newBigState =
  newTVarIO $ BigState {
  appTodoSimple = newTodoApp
}
