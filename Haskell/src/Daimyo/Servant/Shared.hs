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
  appTodoSimpleSTM_Maybe
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
  -- application: todo simple
  :<|> "applications/todo/simple/list" :> Get '[JSON] [Todo]
  :<|> "applications/todo/simple"      :> ReqBody '[JSON] Todo :> Post '[JSON] Todo
  :<|> "applications/todo/simple"      :> Capture "todo_id" TodoId :> Delete '[JSON] (Maybe TodoId)

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
