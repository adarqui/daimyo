module Daimyo.Applications.Todo.Simple where

import Prelude
import Data.Array
import Data.JSON
import Data.Maybe
import Data.Tuple
import Data.Functor
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Maybe.Trans
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Daimyo.Data.Map as M

import Daimyo.Control.Monad
import Daimyo.Data.List

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
  | ReqFindTodoById TodoId
  | ReqClearTodos
  | ReqNoOp

-- | Possible Todo Responses
--
data TodoActionResponse
  = RespListTodos (Array Todo)
  | RespAddTodo Todo
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespFindTodoById (Maybe Todo)
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

instance todoEq :: Eq Todo where
  eq (Todo t1) (Todo t2) =
    t1.todoId == t2.todoId &&
    t1.todoTitle == t2.todoTitle &&
    t1.todoState == t2.todoState

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
todoId :: Todo -> TodoId
todoId (Todo{todoId:tid}) = tid

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
listTodos :: forall eff a. TodoAppState (Array Todo)
listTodos = map snd <$> gets (M.toArray <<< todoAppTodos)

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
  counter <- gets todoAppCounter
  let
    new_id = counter + 1
    todo   = Todo { todoId: new_id, todoTitle: obj.todoTitle, todoState: Active }
  addTodoDirectly todo

-- | addTodoDirectly
--
addTodoDirectly :: forall eff a. Todo -> TodoAppState Todo
addTodoDirectly todo = do
  todos <- gets todoAppTodos
  let
    new_todos = M.insert (todoId todo) todo todos
  put $ TodoApp { todoAppCounter: todoId todo, todoAppTodos: new_todos }
  return todo

-- | removeTodo
--
removeTodo :: forall eff a. TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
--  runMaybeT <<< map go <<< MaybeT <<< findTodoById
--  id <$> findTodoById tid >> go
  todo <- findTodoById tid
  if isNothing todo
     then return Nothing
     else go
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
--  id <$> findTodoById tid >> go
  todo <- findTodoById tid
  if isNothing todo
     then return Nothing
     else go
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

-- | runTodoGrammar
--
-- our todo application grammar in its entirety.
--
runTodoGrammar :: TodoActionRequest -> TodoAppState TodoActionResponse
runTodoGrammar ReqListTodos              = RespListTodos           <$> listTodos
runTodoGrammar (ReqAddTodo todo)         = RespAddTodo             <$> addTodo todo
runTodoGrammar (ReqRemoveTodo tid)       = RespRemoveTodo          <$> removeTodo tid
runTodoGrammar (ReqUpdateTodo tid todo)  = RespUpdateTodo          <$> updateTodo tid todo
runTodoGrammar (ReqFindTodoById tid)     = RespFindTodoById        <$> findTodoById tid
runTodoGrammar ReqClearTodos             = RespClearTodos          <$> clearTodos

runTodoGrammarDirectly :: TodoActionRequest -> TodoAppState TodoActionResponse
runTodoGrammarDirectly (ReqAddTodo todo) = RespAddTodo <$> addTodoDirectly todo
runTodoGrammarDirectly o                 = runTodoGrammar o
