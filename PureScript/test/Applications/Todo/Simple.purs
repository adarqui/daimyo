module Test.Applications.Todo.Simple where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.State
import Control.Monad.Trans
import Data.Tuple
import Data.List
import Data.Maybe
import Pure.Applications.Todo.Simple
import Pure.Monad
import Test.Assert

testTodoSimple = do

  let
    example_todo = defaultTodo "hi"

  log "testing addTodo"
  assert $ (evalState (addTodo example_todo) newTodoApp) == (Todo{todoId:1,todoTitle: "hi",todoState:Active})

  log "testing removeTodo: invalid removal"
  assert $ (evalState (addTodo example_todo >> removeTodo 0) newTodoApp) == Nothing

  log "testing removeTodo: valid removal"
  assert $ (evalState (addTodo example_todo >> removeTodo 1) newTodoApp) == Just 1

  log "testing updateTodo: invalid update"
  assert $ (evalState (addTodo example_todo >> updateTodo 0 example_todo) newTodoApp) == Nothing

  log "testing updateTodo: valid update"
  assert $ (evalState (addTodo example_todo >> updateTodo 1 (Todo{todoId:1,todoTitle:"ih",todoState:Completed})) newTodoApp) == (Just$Todo{todoId:1,todoTitle:"ih",todoState:Completed})
