module Main (
  main,
  module Prelude,
  module Pure.Hello,
  module Pure.NumberTheory.Factorial
) where

import Prelude
import Control.Monad.Eff.Console
import Pure.Hello
import Pure.NumberTheory.Factorial

main = do
  log "main"
