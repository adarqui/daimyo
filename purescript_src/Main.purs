module Main (
  main,
  module Prelude,
  module Daimyo.Hello,
  module Daimyo.NumberTheory.Factorial
) where

import Prelude
import Control.Monad.Eff.Console
import Daimyo.Hello
import Daimyo.NumberTheory.Factorial

main = do
  log "main"
