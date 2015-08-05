module Pure.UI.Halogen.Todo.Simple (
  uiHalogenTodoSimpleMain,
  todosActive,
  todosActiveLength
) where

import Prelude
import Data.Tuple
import Data.Maybe
import Data.JSON
import Data.Array (filter, length, (:))

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Control.Alt
import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Aff

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.HTML.Events.Types as T

import qualified Halogen.HTML.CSS as CSS

import Control.Monad.Aff
import Network.HTTP.Affjax
import Network.HTTP.Method
import Network.HTTP.MimeType
import Network.HTTP.MimeType.Common
import Network.HTTP.RequestHeader

import Pure.Applications.Todo.Simple

data State = State TodoApp (Maybe String)

--type Input = TodoActionRequest
type Input = TodoActionResponse
type Output = TodoActionResponse

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- type HalogenEffects eff = (console :: CONSOLE, ref :: REF, dom :: DOM | eff)
-- type Driver i eff = i -> Eff (HalogenEffects eff) Unit

-- | A `Process` receives inputs and outputs effectful computations which update the DOM.
-- type Process req eff = SF (Tuple req HTMLElement) (Eff (HalogenEffects eff) HTMLElement)

-- type Component m req res = SF1 req (HTML (m res))

-- stateful :: forall s i o. s -> (s -> i -> s) -> SF1 i s
-- stateful' :: forall s i o. s -> (s -> i -> Tuple o s) -> SF i o

class_ s = A.class_ $ A.className s

ui :: forall eff. Component (E.Event (HalogenEffects (ajax :: AJAX | eff))) Input Input
ui = render <$> stateful (State [] Nothing) update
  where
  render :: State -> H.HTML (E.Event (HalogenEffects (ajax :: AJAX | eff)) Input)
  render (State todos new) = appLayout
    where
    appLayout =
      H.section [class_ "todoapp"] [
        H.header [class_ "header"] [
          H.h1_ [H.text "todos"],
          H.input [class_ "new-todo", A.placeholder "What needs to be done?", A.onValueChanged (A.input (RespInput <<< Just)), A.onKeyUp (\e -> pure (handleNewTodo e.keyCode new))] []
        ],
        H.section [class_ "main"] [
          H.input [class_ "toggle-all", A.type_ "checkbox"] [H.label_ [H.text "Mark all as complete"]],
          H.ul [class_ "todo-list"] $ map todoListItem todos,
          H.footer [class_ "footer"] [
            H.span [class_ "todo-count"] [H.strong_ [H.text $ show $ todosActiveLength todos], H.text " items left"],
            H.ul [class_ "filters"] [
              H.li_ [H.text "selected"],
              H.li_ [H.text "active"],
              H.li_ [H.text "completed"]
            ],
            H.button [class_ "clear-completed", A.onClick (\_ -> pure handleClearCompleted)] [H.text "Clear completed"]
          ]
        ],
        H.footer [class_ "info"] [
          H.p_ [H.text "Double-click to edit a todo"],
          H.p_ [H.text "Created by ", H.a [A.href "https://github.com/adarqui/"] [H.text "adarqui"]],
          H.p_ [H.text "Part of ", H.a [A.href "http://todomvc.com"] [H.text "TodoMVC"]]
        ]
      ]

  todoListItem (Todo{todoId=tid, todoTitle=title, todoState=state}) =
    H.li [if state == Completed then class_ "completed" else class_ "active"] [
      H.div [class_ "view"] [
        H.input [class_ "toggle", A.type_ "checkbox", A.checked (state == Completed)] [],
        H.label_ [H.text title],
        H.button [class_ "destroy", A.onClick (\_ -> pure (handleRemoveTodo tid))] []
      ],
      H.input [class_ "edit", A.value title] []
    ]

  update :: State -> Input -> State
  update (State todos new) (RespListTodos xs)      = State xs new
  update (State todos new) (RespAddTodo todo)      = State (todo : todos) new
  update (State todos new) (RespRemoveTodo tid)    = State (filter (\(Todo{todoId=tid'}) -> tid /= tid') todos) new
  update (State todos new) RespClearTodos          = State [] new
  update (State todos new) RespClearCompletedTodos = State [] new
  update (State todos new) (RespInput new')        = State todos new'
  update (State todos _)   RespClearInput          = State todos Nothing
  update st RespNoOp                               = st
  update st RespBusy                               = st

todosActiveLength :: Array Todo -> Int
todosActiveLength = length <<< todosActive

todosActive :: Array Todo -> Array Todo
todosActive todos = filter (\(Todo{ todoState = state}) -> state == Active) todos

handleListTodos :: forall eff. E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleListTodos = E.yield RespBusy `E.andThen` \_ -> E.async affListTodos

handleNewTodo :: forall eff. Number -> Maybe String -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleNewTodo 13.0 Nothing  = return RespNoOp
handleNewTodo 13.0 (Just s) = handleAddTodo $ defaultTodo s
handleNewTodo _    _        = return RespNoOp

handleAddTodo :: forall eff. Todo -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleAddTodo todo = E.yield RespClearInput `E.andThen` \_ -> E.yield RespBusy `E.andThen` \_ -> E.async (affAddTodo todo)

handleRemoveTodo :: forall eff. TodoId -> E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleRemoveTodo tid = E.yield RespBusy `E.andThen` \_ -> E.async (affRemoveTodo tid)

handleClearCompleted :: forall eff. E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handleClearCompleted = E.yield RespBusy `E.andThen` \_ -> E.async affClearCompleted

affListTodos = do
  res <- get "/applications/simple/todos"
  liftEff $ log res.response
  let todos = decode res.response :: Maybe (Array Todo)
  return $ RespListTodos (fromMaybe [] todos)

affAddTodo todo = do
  res <- affjax $ defaultRequest { method = POST, url = "/applications/simple/todos", content = Just (encode (todo :: Todo)), headers = [ContentType applicationJSON] }
  liftEff $ log res.response
  let todo' = decode res.response :: Maybe Todo
  return $ case todo' of
                Nothing   -> RespNoOp
                Just v    -> RespAddTodo v

affRemoveTodo tid = do
  res <- delete ("/applications/simple/todos/" ++ show (tid :: TodoId))
  liftEff $ log res.response
  let tid = decode res.response :: Maybe TodoId
  return $ case tid of
                Nothing   -> RespNoOp
                Just tid' -> RespRemoveTodo tid'

affClearCompleted = do
  res <- delete "/applications/simple/todos"
  liftEff $ log res.response
  let status = decode res.response :: Maybe Boolean
  return $ case status of
                Nothing -> RespNoOp
                Just t  -> if t then RespClearCompletedTodos else RespNoOp

uiHalogenTodoSimpleMain = do
  -- runUI :: forall req eff. Component (Event (HalogenEffects eff)) req req -> Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
  Tuple node driver <- runUI ui
  appendToBody node
  runAff throwException driver affListTodos
