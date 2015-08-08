module Daimyo.Hello (
  helloMain
) where

import Control.Monad.Eff.Console

helloMain = do
  log "hello."
