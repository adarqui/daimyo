module Pure.UI.Halogen.Ping (
  uiHalogenPingMain
) where

import Prelude
import Data.Tuple
import Data.Maybe

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

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E

import qualified Halogen.HTML.CSS as CSS

import Control.Monad.Aff
import Network.HTTP.Affjax

import Pure.Misc.Ping

data State = State (Maybe String)

data Input
  = SetPing String
  | SetBusy
  | SetNothing

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- type HalogenEffects eff = (console :: CONSOLE, ref :: REF, dom :: DOM | eff)
-- type Driver i eff = i -> Eff (HalogenEffects eff) Unit

-- | A `Process` receives inputs and outputs effectful computations which update the DOM.
-- type Process req eff = SF (Tuple req HTMLElement) (Eff (HalogenEffects eff) HTMLElement)

-- type Component m req res = SF1 req (HTML (m res))

-- stateful :: forall s i o. s -> (s -> i -> s) -> SF1 i s
-- stateful' :: forall s i o. s -> (s -> i -> Tuple o s) -> SF i o

ui :: forall eff. Component (E.Event (HalogenEffects (ajax :: AJAX | eff))) Input Input
ui = render <$> stateful (State Nothing) update
  where
  render :: State -> H.HTML (E.Event (HalogenEffects (ajax :: AJAX | eff)) Input)
  render (State v) = H.div_ [layoutHeader, layoutResult v, layoutButtons]

  update :: State -> Input -> State
  update (State _) SetNothing = State Nothing
  update (State _) SetBusy = State (Just "pinging..")
  update (State _) (SetPing s) = State (Just s)

  layoutHeader   = H.p_ [ H.h1_ [ H.text "Ping" ] ]
  layoutResult v = H.p_ [ H.text $ fromMaybe "No response." v ]
  layoutButtons  = H.p_ [ H.button [ A.onClick (\_ -> pure handler) ] [ H.text "ping!" ] ]

empty :: Input
empty = SetNothing

handler :: forall eff. E.Event (HalogenEffects (ajax :: AJAX | eff)) Input
handler = E.yield SetBusy `E.andThen` \_ -> E.async compileAff
  where
  compileAff :: Aff (HalogenEffects (ajax :: AJAX | eff)) Input
  compileAff = do
    res <- get "/ping"
    liftEff $ log res.response
    return $ SetPing res.response

uiHalogenPingMain = do
  -- runUI :: forall req eff. Component (Event (HalogenEffects eff)) req req -> Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
  Tuple node driver <- runUI ui
  appendToBody node
