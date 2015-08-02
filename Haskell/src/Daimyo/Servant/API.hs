{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Daimyo.Servant.API (
  runServer
) where

import           Daimyo.Servant.Shared
import           Daimyo.Servant.API.Ping
import qualified Language.Javascript.JQuery as JQ
import           Network.Wai
import           Network.Wai.Handler.Warp   hiding (Connection)
import           Servant
-- import           Servant.JS

type LnAPI =
       "static"         :> Raw
  :<|> "ping"           :> Get '[JSON] String

daimyoAPI :: Proxy LnAPI
daimyoAPI = Proxy

-- | server
--
server :: Store -> Server LnAPI
server store =
  -- static
       serveDirectory "../../PureScript/"
  -- ping
  :<|> getPing store

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
  writeJSFiles
  let store = ()
  run 31415 $ app store
