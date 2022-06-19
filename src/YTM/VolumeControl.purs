-- | A RecordableSlider with 3 levels of modulation
module YTM.VolumeControl where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int as Int
import Data.Lens ((.~))
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (for_)
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

type State = { settings :: SliderStates
             , opacity :: Opacity
             }

type SliderStates = NonEmptyArray
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

component
 :: forall m
 .  MonadAff m
 => H.Component Query Value Message m
component =
 H.mkComponent
 { initialState
 , render: render
 , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                  , handleQuery = handleQuery
                                  , initialize = Nothing
                                  }
 }

initialState :: Value -> State
initialState value =
  { settings:
    NEA.singleton { volume: value, recording: Nothing, amplitude: 100 } <>
    NEA.singleton { volume: 50, recording: Nothing, amplitude: 100 } <>
    NEA.singleton { volume: 50, recording: Nothing, amplitude: 100 }
  , opacity: 100
  }

render
 :: forall m
 .  MonadAff m
 => State
 -> H.ComponentHTML Action ChildSlots m
render _state = HH.table_
 [ HH.tr_
   [ renderControl 0 "VOL"
   , renderAmplitude 0
   ]
 , HH.tr_
   [ renderControl 1 "SPD"
   , renderAmplitude 1
   ]
 , HH.tr_
   [ renderControl 2 "MOD"
   , renderAmplitude 2
   ]
 , HH.tr_
   [ renderOpacityControl ]
 ]

renderControl
  :: forall m
  .  MonadAff m
  => Int
  -> String
  -> H.ComponentHTML Action ChildSlots m
renderControl idx title =
 HH.td [ HP.class_ (wrap "slider-input-container") ]
 [ HH.slot _controls idx RecordableSlider.component (mkComponentCfg idx)
   (HandleControl idx)
 ]
 where
    -- Volume control
   mkComponentCfg 0 =
     { value: 0
     , from: 0
     , to: 100
     , step: 1.0
     , amplitude: 100
     , defaultValue: 0
     , title
     }
   -- Modulation control
   mkComponentCfg _ =
     { value: 50
     , from: 0
     , to: 100
     , step: 1.0
     , amplitude: 100
     , defaultValue: 50
     , title
     }

renderAmplitude
  :: forall m
  .  MonadAff m
  => Int
  -> H.ComponentHTML Action ChildSlots m
renderAmplitude idx =
 HH.td [ HP.class_ (wrap "slider-input-container") ]
 [ HH.slot _amplitudeControls idx Slider.component cfg
   (HandleAmplitude idx)
 ]
 where
   cfg =
     { value: 100
     , from: 0
     , to: 200
     , step: 1.0
     , defaultValue: 100
     , title: "AMP"
     }

renderOpacityControl
 :: forall m
 .  MonadAff m
 => H.ComponentHTML Action ChildSlots m
renderOpacityControl =
  HH.td
  [ HP.colSpan 2 ]
  [ HH.slot _opacityControl unit Slider.component cfg
    HandleOpacity
  ]
 where
   cfg =
     { value: 100
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
        void $ H.query (Proxy :: Proxy "controls") (idx - 1) $
          H.mkTell (RS.SetPlaybackSpeed $ exp ((Int.toNumber adjustedValue - 50.0) / 12.0))
      newState <- H.modify $ _settings <<< ix idx <<< _volume .~ rawValue
      liftEffect $ Console.log $ show newState
      H.get >>= UpdateRecordingStates >>> H.raise
    RecordableSlider.UpdateValue RecordableSlider.FromPlayback adjustedValue rawValue -> do
      when (idx == 0) do
        raiseValue FromPlayback adjustedValue
      when (idx /= 0) do
        void $ H.query (Proxy :: Proxy "controls") (idx - 1) $
          H.mkTell (RS.SetPlaybackSpeed $ exp ((Int.toNumber adjustedValue - 50.0) / 12.0))
    RecordableSlider.UpdateRecording mbRec -> do
      liftEffect $ Console.log $ show idx <> show mbRec
      H.modify_ $ _settings <<< ix idx <<< _recording .~ mbRec
      H.get >>= UpdateRecordingStates >>> H.raise
handleAction (HandleAmplitude idx (Slider.UpdateValue newAmplitude)) = do
  void $ H.query (Proxy :: Proxy "controls") idx $
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

handleQuery
  :: forall a m i
  .  MonadAff m
  => Query a
  -> H.HalogenM State Action ChildSlots i m (Maybe a)
handleQuery (PutRecordings state a) = do
  H.modify_ $ _settings .~ state.settings
  state.settings `forWithIndex_` \idx { recording: mbRecording, volume, amplitude } -> do
    for_ mbRecording \recording -> do
      H.query _controls idx $ H.mkTell (RecordableSlider.PutRecording recording)
    void $ H.query _controls idx $ H.mkTell (RS.PutValue volume)
    void $ H.query _controls idx $ H.mkTell (RS.PutAmplitude amplitude)
    void $ H.query _amplitudeControls idx $ H.mkTell (Slider.PutValue amplitude)
    void $ H.query _opacityControl unit $ H.mkTell (Slider.PutValue state.opacity)
  pure (Just a)

_volume = prop (Proxy :: Proxy "volume")
_settings = prop (Proxy :: Proxy "settings")
_recording = prop (Proxy :: Proxy "recording")
_amplitude = prop (Proxy :: Proxy "amplitude")
_controls = Proxy :: Proxy "controls"
_amplitudeControls = Proxy :: Proxy "amplitudeControls"
_opacityControl = Proxy :: Proxy "opacityControl"
_opacity = prop (Proxy :: Proxy "opacity")
