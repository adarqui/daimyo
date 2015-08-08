module Daimyo.UI.Thermite.Factorial (
  uiThermiteFactorialMain
) where

import Prelude
import Data.BigInt
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class
import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T
import Daimyo.NumberTheory.Factorial

data Action = Input BigInt | Run | Skip
type State = { input :: BigInt, output :: BigInt }

foreign import getInt :: forall event. event -> Int
foreign import getValue :: forall event. event -> String
foreign import getKeyCode :: T.KeyboardEvent -> Int

handleKeyPress :: T.KeyboardEvent -> Action
handleKeyPress e =
  Input $ fromInt (getInt e)

initialState :: State
initialState = { input: fromInt 0, output: fromInt 1 }

-- type Render eff state props action = Context state action -> state -> props -> Array (Html eff) -> Html eff
render :: T.Render _ State Unit Action
render ctx st props act =
  T.main (A.className "ui page grid")
    [T.div (A.className "row")
      [T.div (A.className "center aligned starter column")
        [T.div' [header, input, output]]]]
  where
  header :: T.Html _
  header =
    T.h1 (A.className "ui header")
      [T.text "Factorial",
        T.div (A.className "sub header")
        [T.text "Calculate factorial"]]
  input :: T.Html _
  input =
    T.div (A.className "ui labeled input")
      [
        T.a (A.className "ui label") [T.text "Input"], T.input (A.placeholder "Enter a number.." <> (T.onKeyUp ctx handleKeyPress)) []
      ]
  output :: T.Html _
  output =
    T.p'
      [
        T.h1' [ T.text "result: ", T.text $ toString st.output ]
      ]
  -- not needed
  buttons :: T.Html _
  buttons =
    T.p'
      [
        T.button (T.onClick ctx (\_ -> Run)) [ T.text "run" ]
      ]

-- type PerformAction eff state props action = props -> action -> Action eff state Unit
performAction :: T.PerformAction _ State Unit Action
performAction _ Run = T.modifyState \st -> { input: st.input, output: factorialBig st.input }
performAction _ (Input i) = T.modifyState \_ -> { input: i, output: factorialBig i }
performAction _ Skip = T.modifyState id

-- newtype Spec eff state props action = Spec (SpecRecord eff state props action)
spec :: T.Spec _ State Unit Action
spec = T.simpleSpec initialState performAction render

-- | uiThermiteFactorialMain
--
-- creates a component for calculating factorial's
--
uiThermiteFactorialMain = do
  let component = T.createClass spec
  T.render component unit
