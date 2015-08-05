module Pure.Applications.Todo.Simple where {- (
  Todo (..),
  TodoState (..),
  TodoActionRequest (..),
  TodoId
) where
-}

import Prelude

type TodoId = Int

data TodoState
  = Active
  | Completed

type Todo = {
  todoId    :: TodoId,
  todoTitle :: String,
  todoState :: TodoState
}

data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
--  | ReqSetTodoActive TodoId
--  | ReqSetTodoCompleted TodoId
--  | ReqFindTodoById TodoId
--  | ReqFindTodosByTitle String
--  | ReqFindActiveTodos
--  | ReqFindCompletedTodos
  | ReqClearTodos
  | ReqBusy
--  | ReqClearCompletedTodos

data TodoActionResponse
  = RespListTodos (Array Todo)
  | RespBusy

type TodoApp = Array Todo

x = false
