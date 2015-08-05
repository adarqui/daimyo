{-# LANGUAGE OverloadedStrings #-}

module Daimyo.Wreq.Todo (
  postSimpleTodo
) where

import           Control.Lens
import           Daimyo.Application.Todo.Simple
import           Data.Aeson
--import           Data.Aeson.Len
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Network.Wreq

postSimpleTodo :: Text -> IO (Maybe Todo)
postSimpleTodo title = do
  r <- post "http://localhost:31415/applications-simple-todos" (toJSON $ defaultTodo title)
  let r' = decode (r ^. responseBody) :: Maybe Todo
  case r' of
    Nothing   -> return Nothing
    otherwise -> return r'
