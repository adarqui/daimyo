module Pure.Applications.Todo.Simple2 where

import Prelude
import Data.List
import Data.JSON
import Data.Maybe
import Data.Tuple
import Data.Functor
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import qualified Data.Map as M

import Pure.Monad

type TodoId = Int

data TodoState
  = Active
  | Completed

data Todo = Todo {
  todoId    :: TodoId,
  todoTitle :: String,
  todoState :: TodoState
}

-- | Available Todo Requests
--
data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
  | ReqClearTodos
  | ReqNoOp

-- | Possible Todo Responses
--
data TodoActionResponse
  = RespListTodos (List Todo)
  | RespAddTodo Todo
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespClearTodos Int
  | RespNoOp

data TodoApp = TodoApp {
  todoAppCounter :: TodoId,
  todoAppTodos   :: M.Map TodoId Todo
}

type TodoAppState a = State TodoApp a

instance todoStateEq :: Eq TodoState where
  eq Active Active       = true
  eq Completed Completed = true
  eq _ _                 = false

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

-- | TodoApp: accessors
--
todoAppCounter :: TodoApp -> TodoId
todoAppCounter (TodoApp{todoAppCounter:counter}) = counter

todoAppTodos :: TodoApp -> M.Map TodoId Todo
todoAppTodos (TodoApp{todoAppTodos:todos}) = todos

-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = TodoApp { todoAppCounter: 0, todoAppTodos: M.empty }

-- | listTodos
--
listTodos :: forall eff a. TodoAppState (List Todo)
listTodos = map snd <$> gets (M.toList <<< todoAppTodos)

-- | clearTodos
--
clearTodos :: forall eff a. TodoAppState Int
clearTodos = do
  sz <- gets (M.size <<< todoAppTodos)
  modify (\(TodoApp obj) -> TodoApp { todoAppCounter: obj.todoAppCounter, todoAppTodos: M.empty })
  return sz

-- | addTodo
--
addTodo :: forall eff a. Todo -> TodoAppState Todo
addTodo (Todo obj) = do
  (TodoApp appObj) <- get
  let
    new_id = appObj.todoAppCounter + 1
    todo   = Todo { todoId: new_id, todoTitle: obj.todoTitle, todoState: Active }
    todos  = M.insert new_id todo appObj.todoAppTodos
  put $ TodoApp { todoAppCounter: new_id, todoAppTodos: todos }
  return todo

-- | removeTodo
--
removeTodo :: forall eff a. TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  id <$> findTodoById tid >> go
  where
  go = do
    (TodoApp appObj) <- get
    let
      todos = M.delete tid appObj.todoAppTodos
    put $ TodoApp { todoAppCounter: appObj.todoAppCounter, todoAppTodos: todos }
    return $ Just tid

-- | updateTodo
--
updateTodo :: forall eff a. TodoId -> Todo -> TodoAppState (Maybe Todo)
updateTodo tid todo = do
  id <$> findTodoById tid >> go
  where
  go = do
    (TodoApp appObj) <- get
    let
      todos = M.update (const $ Just todo) tid appObj.todoAppTodos
    put $ TodoApp { todoAppCounter: appObj.todoAppCounter, todoAppTodos: todos }
    return $ Just todo

-- | findTodoById
--
findTodoById :: forall eff a. TodoId -> TodoAppState (Maybe Todo)
findTodoById tid = gets (M.lookup tid <<< todoAppTodos)

-- | defaultTodo
--
defaultTodo :: String -> Todo
defaultTodo s = Todo { todoId: 0, todoTitle: s, todoState: Active }
