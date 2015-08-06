module Test.Main where

import Control.Monad.Eff.Console
import Test.Applications.Todo.Simple

main = do
  testTodoSimple
