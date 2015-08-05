module Pure.Applications.Todo.Simple where {- (
  Todo (..),
  TodoState (..),
  TodoActionRequest (..),
  TodoId
) where
-}

import Prelude
import Data.JSON

type TodoId = Int

data TodoState
  = Active
  | Completed

data Todo = Todo {
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
  | RespAddTodo Todo
  | RespRemoveTodo TodoId
  | RespUpdateTodo Todo
  | RespClearTodos
  | RespClearCompletedTodos
  | RespBusy
  | RespNoOp

type TodoApp = Array Todo

instance todoStateEq :: Eq TodoState where
  eq Active Active       = true
  eq Completed Completed = true
  eq _ _                 = false

--instance showTodoId :: Show TodoId where
--  show tid = "tid"

instance showTodoState :: Show TodoState where
  show Active    = "Active"
  show Completed = "Completed"

instance todoStateFromJSON :: FromJSON TodoState where
  parseJSON (JString s) =
    case s of
      "Active"    -> return Active
      "Completed" -> return Completed
      _           -> fail "Unknown TodoState"

instance todoStateToJSON :: ToJSON TodoState where
  toJSON Active    = JString "Active"
  toJSON Completed = JString "Completed"

instance showTodo :: Show Todo where
  show (Todo {todoId=tid, todoTitle=title, todoState=state}) =
    "Todo { todoId = " ++show tid ++ ", todoTitle = \"" ++ title ++ "\", todoState = " ++ show state

instance todoFromJSON :: FromJSON Todo where
  parseJSON (JObject o) = do
    tid <- o .: "todoId"
    title <- o .: "todoTitle"
    state <- o .: "todoState"
    return $ Todo { todoId: tid, todoTitle: title, todoState: state }
  parseJSON _ = fail "Invalid Todo"

instance todoToJSON :: ToJSON Todo where
  toJSON (Todo { todoId = tid, todoTitle = title, todoState = state }) =
    object [ "todoId" .= tid, "todoTitle" .= title, "todoState" .= state ]
