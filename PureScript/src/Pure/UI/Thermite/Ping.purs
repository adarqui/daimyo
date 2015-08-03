module Pure.UI.Thermite.Ping (
  uiPingMain
) where

import Prelude hiding (append)
import DOM
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Network.HTTP.Method
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T
import Control.Monad.Eff.JQuery

import Network.HTTP.Method
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response

import Pure.Misc.Ping (pingAjax)

type State = { resp :: String }

initialState :: State
initialState = { resp: "unknown" }

-- type Render eff state props action = Context state action -> state -> props -> Array (Html eff) -> Html eff
render :: T.Render _ State _ _
render ctx s _ _ = T.div' [ping, buttons]
  where
  ping :: T.Html _
  ping =
    T.p'
      [ T.text "Ping Response: "
      , T.text $ show s.resp
      ]

  buttons :: T.Html _
  buttons =
    T.p'
      [ T.button (T.onClick ctx (\_ -> "hi"))
--    [    T.button (T.onClick ctx (\_ -> T.runAction ctx (T.modifyState \st -> { resp: "yo" })))
                 [ T.text "Ping" ]
      ]

-- type PerformAction eff state props action = props -> action -> Action eff state Unit
performAction :: T.PerformAction _ State _ _
performAction _ _ = do
  T.modifyState \st -> { resp: "hi!" }

-- newtype Spec eff state props action = Spec (SpecRecord eff state props action)
spec :: T.Spec _ State _ _
spec = T.simpleSpec initialState performAction render

uiPingMain = do
  let component = T.createClass spec
  T.render component {}

-- render :: forall props eff. ComponentClass props eff -> props -> Eff (dom :: DOM | eff) Unit
