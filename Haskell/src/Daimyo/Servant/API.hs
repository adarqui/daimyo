{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Daimyo.Servant.API (
  runServer
) where

import           Control.Concurrent.STM
import           Control.Monad.Trans.Either
import           Daimyo.Application.Todo.Simple
import           Daimyo.Servant.Shared
import           Daimyo.Servant.API.Ping
import qualified Language.Javascript.JQuery as JQ
import           Network.Wai
import           Network.Wai.Handler.Warp   hiding (Connection)
import           Servant

-- | server
--
server :: Store -> Server LnAPI
server store =
  -- static
       serveDirectory "../"
  -- ping
--  :<|> getPing store
  :<|> getPing store
  -- application: todo simple
  :<|> appTodoSimpleSTM store listTodos
  :<|> appTodoSimpleSTM store . addTodo
  :<|> appTodoSimpleSTM store clearTodos
  :<|> appTodoSimpleSTM_Maybe store . findTodoById
  :<|> appTodoSimpleSTM_Maybe store . removeTodo
  :<|> apply2 updateTodo store -- bleh
  :<|> appTodoSimpleSTM_Maybe store . setTodoActive
  :<|> appTodoSimpleSTM_Maybe store . setTodoCompleted

-- | app
--
app :: Store -> Application
app store = serve daimyoAPI $ server store

-- | appJS
--
--apiJS :: String
--apiJS = jsForAPI daimyoAPI jquery

-- | writeJSFiles
--
writeJSFiles :: IO ()
writeJSFiles = do
  -- writeFile "build/js/api.js" apiJS
  jq <- readFile =<< JQ.file
  writeFile "build/js/jq.js" jq

-- | runServer
--
-- runs the API servers on:
-- http://localhost:8080
--
-- connects to temporary redis (ardb) on:
--
runServer :: IO ()
runServer = do
  -- writeJSFiles
  store <- newBigState
  run 31415 $ app store
