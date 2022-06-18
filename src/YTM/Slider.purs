module YTM.Slider where

import Prelude

import Control.MonadZero (guard)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Time.Duration (negateDuration)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (throw)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element as DOM.Element
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import YTM.Types (RecordingEntry)

type Value = Int

type State = { value :: Value
             , from :: Value
             , to :: Value
             , step :: Number
             , defaultValue :: Value
             , title :: String
             }

data Action
  = ChangeValue String
  | ChangeValueMouseMove MouseEvent

data Query a
  = PutValue Value a

data Message
  = UpdateValue Value
  -- ^ Tell parent to update value

type Slot = H.Slot Query Message

type Params =
  { value :: Value
  , from :: Value
  , to :: Value
  , step :: Number
  , defaultValue :: Value
  , title :: String
  }

component
  :: forall m
  .  MonadAff m
  => H.Component Query Params Message m
component =
  H.mkComponent
  { initialState
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , handleQuery = handleQuery
                                   , initialize = Nothing
                                   }
  }

initialState :: Params -> State
initialState { value, from, to, defaultValue, step, title } =
  { value
  , from
  , to
  , step
  , defaultValue
  , title
  }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.table_
  [ HH.tr_
    [ HH.td [HP.class_ (wrap "controller-title") ]
      [ HH.text state.title ]
    , HH.td_ [ renderSlider state ]
    ]
  ]

renderSlider :: forall cs m. State -> H.ComponentHTML Action cs m
renderSlider state =
  HH.input
  [ HP.type_ HP.InputRange
  , HP.class_ (wrap "slider-input")
  , HP.min $ Int.toNumber state.from
  , HP.step $ HP.Step state.step
  , HP.max $ Int.toNumber state.to
  , HP.value (show $ state.value)
  , HE.onValueChange ChangeValue
  , HE.onMouseMove $ ChangeValueMouseMove
  ]

handleAction
  :: forall m cs
  .  MonadAff m
  => Action -> H.HalogenM State Action cs Message m Unit
handleAction = case _ of
  ChangeValue valueStr -> do
    from <- H.gets _.from
    to <- H.gets _.to
    for_ (valueFromString from to valueStr) \value -> do
      raiseValue value
  ChangeValueMouseMove mouseEvent -> do
    value <- getValueFromEvent mouseEvent
    void $ raiseValue value

mkEntry :: Instant -> Instant -> Value -> RecordingEntry
mkEntry nowTime lastTime value =
  { relTime:
    -- now - last
    unInstant nowTime <> negateDuration (unInstant lastTime)
  , value: value }

-- | Tell the parent component a new value
raiseValue
  :: forall m cs
  .  MonadAff m
  => Value
  -> H.HalogenM State Action cs Message m Unit
raiseValue value = do
  oldValue <- H.gets _.value
  let shouldUpdate = oldValue /= value
  when shouldUpdate do
    H.modify_ _ { value = value }
    H.raise (UpdateValue value)
  pure unit

getValueFromEvent
  :: forall m cs rest action message
  .  MonadAff m
  => MouseEvent
  -> H.HalogenM { from :: Value, to :: Value | rest } action cs message m Value
getValueFromEvent mouseEvent = do
  let
    mbInputElement =
      Event.target (MouseEvent.toEvent mouseEvent) >>=
      (DOM.Element.fromEventTarget >=>
       HTMLInputElement.fromElement)
  from <- H.gets _.from
  to <- H.gets _.to
  case mbInputElement of
    Nothing -> liftEffect $ throw "No target element for given event"
    Just input -> do
      mbValue <- liftEffect (HTMLInputElement.value input) <#> valueFromString from to
      maybe (liftEffect $ throw $ "No value parsed from string") pure mbValue

valueFromString :: Value -> Value -> String -> Maybe Value
valueFromString from to valueStr = do
  int <- Int.fromString valueStr
  guard (int >= from && int <= to)
  pure int

handleQuery
  :: forall a slots m
  .  MonadAff m
  => Query a
  -> H.HalogenM State Action slots Message m (Maybe a)
handleQuery (PutValue value a) = do
  H.modify_ _ { value = value }
  pure $ Just a
