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
  newBigState,
  appTodoSimpleSTM,
  appTodoSimpleSTM_Maybe,
  apply2
) where

--
-- This file will contain a ton of state.. since all of our example apis will be loaded
--

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Daimyo.Application.Todo.Simple
import           Daimyo.Control.State
import           Servant

data BigState = BigState {
  appTodoSimple :: TodoApp
}

type Store = TVar BigState

type LnAPI =
       "static"         :> Raw
  :<|> "ping"           :> Get '[JSON] String
  -- application: simple todos
  -- GET /applications/simple/todos
  -- POST /applications/simple/todos , body = Todo
  -- DELETE /applications/simple/todos
  -- GET /applications/simple/todos/:todo_id
  -- DELETE /applications/simple/todos/:todo_id
  -- PUT /applications/simple/todos/:todo_id , body = Todo
  -- PUT /applications/simple/todos/active/:todo_id
  -- PUT /applications/simple/todos/completed/:todo_id
  :<|> "applications-simple-todos"                 :> Get '[JSON] [Todo]
  :<|> "applications-simple-todos"                 :> ReqBody '[JSON] Todo :> Post '[JSON] Todo
  :<|> "applications-simple-todos"                 :> Delete '[JSON] Bool
  :<|> "applications-simple-todos"                 :> Capture "todo_id" TodoId :> Get '[JSON] Todo
  :<|> "applications-simple-todos"                 :> Capture "todo_id" TodoId :> Delete '[JSON] TodoId
  :<|> "applications-simple-todos"                 :> Capture "todo_id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] Todo
  :<|> "applications-simple-todos-state-active"    :> Capture "todo_id" TodoId :> Put '[JSON] Todo
  :<|> "applications-simple-todos-state-completed" :> Capture "todo_id" TodoId :> Put '[JSON] Todo

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

-- | appTodoSimpleSTM
--
-- simple todo application helper
--
appTodoSimpleSTM :: MonadIO m => Store -> State TodoApp b -> EitherT ServantErr m b
appTodoSimpleSTM store cb = do
  liftIO $ atomically $ do
    v <- readTVar store
    let (a, s) = runState cb (appTodoSimple v)
    writeTVar store (v { appTodoSimple = s })
    return a

-- | appTodoSimpleSTM_Maybe
--
-- returns an error if our todo action returns Nothing
--
appTodoSimpleSTM_Maybe :: MonadIO m => Store -> State TodoApp (Maybe b) -> EitherT ServantErr m b
appTodoSimpleSTM_Maybe store cb = do
  s <- appTodoSimpleSTM store cb
  case s of
    Nothing -> left err400
    Just v  -> return v

-- | apply2
--
-- bleh: having some weird type errors
--
apply2 f s x y = appTodoSimpleSTM_Maybe s (f x y)
