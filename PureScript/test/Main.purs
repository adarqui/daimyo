module Test.Main where

import Control.Monad.Eff.Console
import Test.Daimyo.Applications.Todo.Simple

main = do
  testTodoSimple
