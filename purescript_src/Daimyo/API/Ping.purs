module Daimyo.API.Ping (
  pingAjax,
  pingAjaxAff
) where

import Prelude hiding (append)
import DOM
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Network.HTTP.Method
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response

pingAjax :: forall e. Control.Monad.Aff.Aff (ajax :: Network.HTTP.Affjax.AJAX | e) Prim.String
pingAjax = do
  res <- affjax $ defaultRequest { url = "/ping", method = GET }
  liftEff $ log res.response
  return $ show res.response

pingAjaxAff :: forall e. Control.Monad.Eff.Eff (ajax :: Network.HTTP.Affjax.AJAX | e) Unit
pingAjaxAff = launchAff $ do
  res <- affjax $ defaultRequest { url = "/ping", method = GET }
  liftEff $ log res.response
  return unit
