-- | A RecordableSlider with 3 levels of modulation
module YTM.VolumeControl where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int as Int
import Data.Lens (Lens', (.~), (^?))
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Math (exp)
import Type.Proxy (Proxy(..))
import YTM.Constants (maxOpacity)
import YTM.RecordableSlider (ValueUpdateType(..))
import YTM.RecordableSlider as RS
import YTM.RecordableSlider as RecordableSlider
import YTM.Slider (Value)
import YTM.Slider as Slider
import YTM.Types (Recording, Volume, Opacity)

type State = { settings :: NonEmptyArray SliderState
             , opacity :: Opacity
             }

type SliderStates = NonEmptyArray SliderState

type SliderState =
  { volume :: Volume
  , recording :: Maybe Recording
  , amplitude :: Value
  }

type SliderIndex = Int

data Action
  = HandleControl SliderIndex RecordableSlider.Message
  -- ^ Handle volume control or volume automation control
  | HandleAmplitude SliderIndex Slider.Message
  -- ^ Handle amplitude control
  | HandleOpacity Slider.Message

type ChildSlots =
  ( controls :: RecordableSlider.Slot Int
  , amplitudeControls :: Slider.Slot Int
  , opacityControl :: Slider.Slot Unit
  )

data Message
 = UpdateValue ValueUpdateType Value
 | UpdateRecordingStates State
 | UpdateOpacity Opacity

type Slot = H.Slot Query Message

data Query a
 = PutRecordings State a

type Label = String

component
 :: forall m
 .  MonadAff m
 => H.Component Query State Message m
component =
 H.mkComponent
 { initialState
 , render: render
 , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                  , handleQuery = handleQuery
                                  , initialize = Nothing
                                  }
 }

initialState :: State -> State
initialState _value = _value

render
 :: forall m
 .  MonadAff m
 => State
 -> H.ComponentHTML Action ChildSlots m
render state = HH.table_ $
  Array.zipWith mkController
  (Array.fromFoldable state.settings)
  [ 0 /\ "VOL" /\ volumeTitle, 1 /\ "SPD" /\ speedTitle, 2 /\ "MOD" /\ modTitle ] <>
  [ HH.tr_
    [ renderOpacityControl state.opacity ]
  ]
  where
    mkController :: SliderState -> (SliderIndex /\ Label /\ String) -> H.ComponentHTML Action ChildSlots m
    mkController { volume, recording, amplitude } (idx /\ label /\ title) =
      HH.tr_
      [ renderControl idx label volume recording title
      , renderAmplitude idx amplitude
      ]
    volumeTitle = "Video volume"
    speedTitle = "Speed of video volume automation playback"
    modTitle = "Modulation of automation speed of the above slider"

renderControl
  :: forall m
  .  MonadAff m
  => Int
  -> Label
  -> Volume
  -> Maybe Recording
  -> String
  -> H.ComponentHTML Action ChildSlots m
renderControl idx title volume recording elTitle =
  HH.td [ HP.class_ (wrap "slider-input-container"), HP.title elTitle ]
  [ HH.slot _controls idx RecordableSlider.component (mkComponentCfg idx)
    (HandleControl idx)
  ]
  where
    -- Volume control
    mkComponentCfg 0 =
      { value: volume
      , from: 0
      , to: 100
      , step: 1.0
      , amplitude: 100
      , defaultValue: 0
      , title
      , recording: recording
      }
    -- Modulation control
    mkComponentCfg _ =
      { value: volume
      , from: 0
      , to: 100
      , step: 1.0
      , amplitude: 100
      , defaultValue: 50
      , title
      , recording: recording
      }

renderAmplitude
  :: forall m
  .  MonadAff m
  => Int
  -> Int
  -> H.ComponentHTML Action ChildSlots m
renderAmplitude idx amplitude =
 HH.td [ HP.class_ (wrap "slider-input-container"), HP.title "Amplitude of the slider to the left"]
 [ HH.slot _amplitudeControls idx Slider.component cfg
   (HandleAmplitude idx)
 ]
 where
   cfg =
     { value: amplitude
     , from: 0
     , to: 200
     , step: 1.0
     , defaultValue: 100
     , title: "AMP"
     }

renderOpacityControl
 :: forall m
 .  MonadAff m
 => Opacity
 -> H.ComponentHTML Action ChildSlots m
renderOpacityControl opacity =
  HH.td
  [ HP.colSpan 2, HP.title "Video opacity coefficient" ]
  [ HH.slot _opacityControl unit Slider.component cfg
    HandleOpacity
  ]
 where
   cfg =
     { value: opacity
     , from: 0
     , to: maxOpacity
     , step: 1.0
     , defaultValue: 100
     , title: "OPC"
     }

handleAction
 :: forall m
 .  MonadAff m
 => Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction (HandleControl idx msg) =
  case msg of
    RecordableSlider.UpdateValue RecordableSlider.FromUser adjustedValue rawValue -> do
      if idx == 0
        then raiseValue FromUser adjustedValue
        else do
        setPlaybackSpeed (idx - 1) adjustedValue
      newState <- H.modify $ _settings <<< ix idx <<< _volume .~ rawValue
      liftEffect $ Console.log $ show newState
      H.get >>= UpdateRecordingStates >>> H.raise
    RecordableSlider.UpdateValue RecordableSlider.FromPlayback adjustedValue _ -> do
      when (idx == 0) do
        raiseValue FromPlayback adjustedValue
      when (idx /= 0) do
        setPlaybackSpeed (idx - 1) adjustedValue
    RecordableSlider.UpdateRecording mbRec -> do
      liftEffect $ Console.log $ show idx <> show mbRec
      H.modify_ $ _settings <<< ix idx <<< _recording .~ mbRec
      H.get >>= UpdateRecordingStates >>> H.raise
handleAction (HandleAmplitude idx (Slider.UpdateValue newAmplitude)) = do
  void $ H.query _controls idx $
    H.mkTell (RS.PutAmplitude newAmplitude)
  H.modify_ $ _settings <<< ix idx <<< _amplitude .~ newAmplitude
  H.get >>= UpdateRecordingStates >>> H.raise
handleAction (HandleOpacity (Slider.UpdateValue newOpacity)) = do
  H.modify_ _ { opacity = newOpacity }
  H.raise (UpdateOpacity newOpacity)
  H.get >>= UpdateRecordingStates >>> H.raise

-- | Raise value multipliying it by current amplitude
raiseValue
  :: forall m
  .  MonadAff m
  => RecordableSlider.ValueUpdateType
  -> Value
  -> H.HalogenM State Action ChildSlots Message m Unit
raiseValue updateType value = do
  H.raise $ UpdateValue updateType value

setPlaybackSpeed
  :: forall m state message action
  .  MonadAff m
  => SliderIndex
  -> Value
  -> H.HalogenM state action ChildSlots message m Unit
setPlaybackSpeed idx value = do
  void $ H.query _controls idx $
    H.mkTell $ RS.SetPlaybackSpeed $
    exp ((Int.toNumber value - 50.0) / 12.0)

handleQuery
  :: forall a m
  .  MonadAff m
  => Query a
  -> H.HalogenM State Action ChildSlots Message m (Maybe a)
handleQuery (PutRecordings state a) = do
  H.modify_ $ _settings .~ state.settings
  state.settings `forWithIndex_` \idx { recording: mbRecording, volume, amplitude } -> do
    void $ H.query _opacityControl unit $ H.mkTell (Slider.PutValue state.opacity)
    void $ H.query _controls idx $ H.mkTell (RS.PutAmplitude amplitude)
    void $ H.query _amplitudeControls idx $ H.mkTell (Slider.PutValue amplitude)
    setPlaybackSpeed (idx - 1) volume
  H.raise $ UpdateValue FromPlayback $ fromMaybe 0 $ state.settings ^? ix 0 <<< _volume
  H.raise (UpdateOpacity $ state.opacity)
  pure (Just a)

_volume :: forall rest a. Lens' { volume :: a | rest} a
_volume = prop (Proxy :: Proxy "volume")
_settings = prop (Proxy :: Proxy "settings")
_recording = prop (Proxy :: Proxy "recording")
_amplitude = prop (Proxy :: Proxy "amplitude")
_controls = Proxy :: Proxy "controls"
_amplitudeControls = Proxy :: Proxy "amplitudeControls"
_opacityControl = Proxy :: Proxy "opacityControl"
_opacity = prop (Proxy :: Proxy "opacity")
