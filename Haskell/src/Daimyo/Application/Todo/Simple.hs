{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--
-- really simple todo impl.
-- todo (ironically): type level safety between requests and responses
--

module Daimyo.Application.Todo.Simple (
  Todo (..),
  TodoActionRequest (..),
  TodoActionResponse (..),
  TodoState (..),
  TodoApp (..),
  TodoAppState,
  TodoId,
  newTodoApp,
  defaultTodo,
  listTodos,
  addTodo,
  removeTodo,
  updateTodo,
  setTodoActive,
  setTodoCompleted,
  setTodoState,
  findTodoById,
  findTodosByTitle,
  findActiveTodos,
  findCompletedTodos,
  findTodosByState,
  clearTodos,
  clearCompletedTodos,
  runTodoGrammar,
  incrTodoAppCounter
) where

import           Control.Applicative
import           Control.Monad
import           Daimyo.Control.State
import           Daimyo.Tree.AVLKV
import           Data.Aeson
import           Data.List
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics

type TodoId = Int

data Todo = Todo {
  todoId    :: TodoId,
  todoTitle :: Text,
  todoState :: TodoState
} deriving (Show, Eq, Ord, Generic)

data TodoActionRequest
  = ReqListTodos
  | ReqAddTodo Todo
  | ReqRemoveTodo TodoId
  | ReqUpdateTodo TodoId Todo
  | ReqSetTodoActive TodoId
  | ReqSetTodoCompleted TodoId
  | ReqFindTodoById TodoId
  | ReqFindTodosByTitle Text
  | ReqFindActiveTodos
  | ReqFindCompletedTodos
  | ReqClearTodos
  | ReqClearCompletedTodos
  deriving (Show, Eq, Ord, Generic)

data TodoActionResponse
  = RespListTodos [Todo]
  | RespAddTodo (Maybe Todo)
  | RespRemoveTodo (Maybe TodoId)
  | RespUpdateTodo (Maybe Todo)
  | RespSetTodoActive (Maybe Todo)
  | RespSetTodoCompleted (Maybe Todo)
  | RespFindTodoById (Maybe Todo)
  | RespFindTodosByTitle [Todo]
  | RespFindActiveTodos [Todo]
  | RespFindCompletedTodos [Todo]
  | RespClearTodos Bool
  | RespClearCompletedTodos Bool
  deriving (Show, Eq, Ord, Generic)

data TodoState
  = Active
  | Completed
  deriving (Show, Eq, Ord, Generic)

data TodoApp = TodoApp {
  todoAppTodos   :: [Todo],
  todoAppCounter :: TodoId
} deriving (Show, Eq, Ord, Generic)

type TodoAppState a = State TodoApp a

instance FromJSON Todo
instance ToJSON Todo

instance FromJSON TodoState
instance ToJSON TodoState

-- | newTodoApp
--
newTodoApp :: TodoApp
newTodoApp = TodoApp [] 0

-- | listTodos
--
listTodos :: TodoAppState [Todo]
listTodos = gets todoAppTodos >>= return

-- | addTodo
--
addTodo :: Todo -> TodoAppState Todo
addTodo todo = do
  new_id <- incrTodoAppCounter
  let
    todo' = todo { todoId = new_id }
  modify (\st -> st { todoAppTodos = (todo' : todoAppTodos st) })
  return todo'

-- | removeTodo
--
removeTodo :: TodoId -> TodoAppState (Maybe TodoId)
removeTodo tid = do
  todos <- gets todoAppTodos
  let
    e      = find (\todo -> todoId todo == tid) todos
    todos' = filter (\todo -> todoId todo /= tid) todos
  if e == Nothing
     then return Nothing
     else modify (\st -> st { todoAppTodos = todos' }) >> (return $ Just tid)

-- | updateTodo
--
updateTodo :: TodoId -> Todo -> TodoAppState (Maybe Todo)
updateTodo tid new_todo = do
  let new_todo' = new_todo { todoId = tid }
  todo <- findTodoById tid
  case todo of
    Nothing    -> return Nothing
    Just todo' -> do
      todos <- gets todoAppTodos
      let filtered = filter (\todo -> todoId todo /= tid) todos
      modify (\st -> st { todoAppTodos = new_todo' : filtered })
      return $ Just new_todo'

-- | setTodoActive
--
setTodoActive :: TodoId -> TodoAppState (Maybe Todo)
setTodoActive = setTodoState Active

-- | setTodoCompleted
--
setTodoCompleted :: TodoId -> TodoAppState (Maybe Todo)
setTodoCompleted = setTodoState Completed

-- | setTodoState
--
setTodoState :: TodoState -> TodoId -> TodoAppState (Maybe Todo)
setTodoState tst tid = do
  todo <- findTodoById tid
  case todo of
    Nothing    -> return Nothing
    Just todo' -> updateTodo tid (todo' { todoState = tst })

-- | findTodoById
--
findTodoById :: TodoId -> TodoAppState (Maybe Todo)
findTodoById tid = do
  todos <- gets todoAppTodos
  return $ find (\todo -> todoId todo == tid) todos

-- | findTodosByTitle
--
findTodosByTitle :: Text -> TodoAppState [Todo]
findTodosByTitle s = do
  todos <- gets todoAppTodos
  let
    filtered = filter (\todo -> T.isInfixOf (T.toLower s) (T.toLower $ todoTitle todo)) todos
  return filtered

-- | findActiveTodos
--
findActiveTodos :: TodoAppState [Todo]
findActiveTodos = findTodosByState Active

-- | findCompletedTodos
--
findCompletedTodos :: TodoAppState [Todo]
findCompletedTodos = findTodosByState Completed

-- | findTodosByState
--
findTodosByState :: TodoState -> TodoAppState [Todo]
findTodosByState tst = do
  todos <- gets todoAppTodos
  let
    filtered = filter (\todo -> todoState todo == tst) todos
  return filtered

-- | clearTodos
--
clearTodos :: TodoAppState Bool
clearTodos = do
  modify (\st -> st { todoAppTodos = [] })
  return True

-- | clearCompletedTodos
--
clearCompletedTodos :: TodoAppState Bool
clearCompletedTodos = do
  modify (\st -> st { todoAppTodos = filter (\todo -> todoState todo /= Completed) (todoAppTodos st) })
  return True

-- | incrTodoAppCounter
--
-- >>> runState (incrTodoAppCounter >> incrTodoAppCounter) newTodoApp
-- (2,TodoApp {todoAppTodos = [], todoAppCounter = 2})
--
incrTodoAppCounter :: TodoAppState TodoId
incrTodoAppCounter = do
  counter <- gets todoAppCounter
  let
    new_counter = counter + 1
  modify (\st -> st { todoAppCounter = new_counter })
  return new_counter

-- | defaultTodo
--
-- >>> defaultTodo "hi!"
-- Todo {todoId = 0, todoTitle = "hi!", todoState = Active}
--
defaultTodo :: Text -> Todo
defaultTodo title = Todo 0 title Active

-- | runTodoGrammar
--
-- our todo application grammar in its entirety.
--
runTodoGrammar :: TodoActionRequest -> TodoAppState TodoActionResponse
runTodoGrammar ReqListTodos              = RespListTodos           <$> listTodos
runTodoGrammar (ReqAddTodo todo)         = (RespAddTodo . Just)    <$> addTodo todo
runTodoGrammar (ReqRemoveTodo tid)       = RespRemoveTodo          <$> removeTodo tid
runTodoGrammar (ReqUpdateTodo tid todo)  = RespUpdateTodo          <$> updateTodo tid todo
runTodoGrammar (ReqSetTodoActive tid)    = RespSetTodoActive       <$> setTodoState Active tid
runTodoGrammar (ReqSetTodoCompleted tid) = RespSetTodoCompleted    <$> setTodoState Completed tid
runTodoGrammar (ReqFindTodoById tid)     = RespFindTodoById        <$> findTodoById tid
runTodoGrammar (ReqFindTodosByTitle s)   = RespFindTodosByTitle    <$> findTodosByTitle s
runTodoGrammar ReqFindActiveTodos        = RespFindActiveTodos     <$> findTodosByState Active
runTodoGrammar ReqFindCompletedTodos     = RespFindCompletedTodos  <$> findTodosByState Completed
runTodoGrammar ReqClearTodos             = RespClearTodos          <$> clearTodos
runTodoGrammar ReqClearCompletedTodos    = RespClearCompletedTodos <$> clearCompletedTodos


-- | newTodoApp
--
-- >>> newTodoApp
-- TodoApp {todoAppTodos = [], todoAppCounter = 0}
--

-- | listTodos
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> listTodos) newTodoApp
-- ([Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | addTodo
-- >>> runState (addTodo (defaultTodo "write a purescript todo app")) newTodoApp
-- (Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active},TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | removeTodo
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> removeTodo 1) newTodoApp
-- (Just 1,TodoApp {todoAppTodos = [], todoAppCounter = 1})
--

-- | updateTodo
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >>= \todo' -> updateTodo 1 (todo'{todoTitle="hey"})) newTodoApp
-- (Just (Todo {todoId = 1, todoTitle = "hey", todoState = Active}),TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "hey", todoState = Active}], todoAppCounter = 1})
--

-- | setTodoActive
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> setTodoCompleted 1 >> setTodoActive 1) newTodoApp
-- (Just (Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}),TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | setTodoCompleted
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> setTodoCompleted 1) newTodoApp
-- (Just (Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Completed}),TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Completed}], todoAppCounter = 1})
--

-- | setTodoState
--

-- | findTodoById
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> findTodoById 1) newTodoApp
-- (Just (Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}),TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | findTodosByTitle
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> findTodosByTitle "Pure") newTodoApp
-- ([Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> findTodosByTitle "derp") newTodoApp
-- ([],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | findTodosByState
--

-- | findActiveTodos
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> findActiveTodos) newTodoApp
-- ([Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> findCompletedTodos) newTodoApp
-- ([],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | findCompletedTodos
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> setTodoCompleted 1 >> findCompletedTodos) newTodoApp
-- ([Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Completed}],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Completed}], todoAppCounter = 1})
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> setTodoCompleted 1 >> findActiveTodos) newTodoApp
-- ([],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Completed}], todoAppCounter = 1})
--

--
-- | clearTodos
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> clearTodos) newTodoApp
-- (True,TodoApp {todoAppTodos = [], todoAppCounter = 1})
--

-- | clearCompleted
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> setTodoCompleted 1 >> clearCompletedTodos) newTodoApp
-- (True,TodoApp {todoAppTodos = [], todoAppCounter = 1})
--
-- >>> runState (addTodo (defaultTodo "write a purescript todo app") >> clearCompletedTodos) newTodoApp
-- (True,TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--

-- | Examples
--
-- >>> runState (addTodo $ defaultTodo "write a purescript todo app") newTodoApp
-- (Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active},TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--
-- >>> runState (runTodoGrammar (ReqAddTodo (defaultTodo "write a purescript todo app")) >> runTodoGrammar ReqListTodos) newTodoApp
-- (RespListTodos [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}],TodoApp {todoAppTodos = [Todo {todoId = 1, todoTitle = "write a purescript todo app", todoState = Active}], todoAppCounter = 1})
--
-- >>> runState (runTodoGrammar (ReqAddTodo (defaultTodo "write a purescript todo app")) >> runTodoGrammar ReqListTodos >> runTodoGrammar (ReqRemoveTodo 1)) newTodoApp
-- (RespRemoveTodo (Just 1),TodoApp {todoAppTodos = [], todoAppCounter = 1})
