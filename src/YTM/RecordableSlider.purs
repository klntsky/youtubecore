module YTM.RecordableSlider where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (negateDuration)
import Data.Traversable (for_)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (now)
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent, ctrlKey, shiftKey)
import YTM.Slider (Value, getValueFromEvent, valueFromString)
import YTM.Types (RecordingEntry, Recording)

-- | Time of the previous recording frame
type LastTime = Instant

data RecorderState
  = Inactive
  | ReadyToRecord
  | Recording LastTime (List RecordingEntry)

derive instance Generic RecorderState _

instance Show RecorderState where
  show = genericShow

data PlaybackState
  = PlaybackPlaying (Array RecordingEntry) H.ForkId
  | PlaybackReady Recording
  -- TODO: | PlaybackOnHold we started recording while playback was active
  | PlaybackPaused (Maybe (Array RecordingEntry))

type State = { recorderState :: RecorderState
             , playbackState :: PlaybackState
             , lastSentValue :: Value
             , lastAmplitude :: Int
             -- ^ Value to show on slider in UI, also used to prevent excessive updates
             , constantValue :: Value
             , playbackSpeed :: Number
             , from :: Value
             , to :: Value
             , amplitude :: Int
             , step :: Number
             , defaultValue :: Value
             , title :: String
             }

data Action
  = GetReadyToRecord
  | CancelRecording
  | MouseDown MouseEvent
  | StopRecordingAndSave MouseEvent
  | ChangeValue String
  | ChangeValueMouseMove MouseEvent
  | PlaybackPause
  | PlaybackResume
  | DeleteRecording
  | Init

type IsPlaying = Boolean

data Query a
  = SetValueFromPlayer Value (IsPlaying -> a)
  -- ^ Indicates that the user manually changed the value, provides IsPlaying flag back.
  | PutAmplitude Int a
  | SetPlaybackSpeed Number a

data ValueUpdateType
  = FromUser
  -- ^ Indicates that the value has been changed because the user moved the slider
  | FromPlayback
  -- ^ Indicates that the value has been changed because of running playback

type AdjustedValue = Value

data Message
  = UpdateValue ValueUpdateType AdjustedValue Value
  -- ^ Tell parent to update value
  | UpdateRecording (Maybe (Array RecordingEntry))
  -- ^ Tell parent that a new recording has been assigned

type Slot = H.Slot Query Message

type Params =
  { value :: Value
  , from :: Value
  , to :: Value
  , step :: Number
  , amplitude :: Int
  , defaultValue :: Value
  , title :: String
  , recording :: Maybe Recording
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
                                   , initialize = Just Init
                                   }
  }

initialState :: Params -> State
initialState { value, from, to, defaultValue, step, title, amplitude, recording } =
  { recorderState: Inactive
  , playbackState: case recording of
    Nothing -> PlaybackPaused Nothing
    Just rec -> PlaybackReady rec
  , lastSentValue: value
  , lastAmplitude: amplitude
  , constantValue: value
  , playbackSpeed: 1.0
  , from
  , to
  , step
  , amplitude
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
    , HH.td_ [ renderRecorderButton state ]
    , HH.td_ [ renderPlaybackButton state ]
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
  , HP.value (show $ state.lastSentValue)
  , HE.onValueChange ChangeValue
  , HE.onMouseDown $ MouseDown
  , HE.onMouseUp $ StopRecordingAndSave
  , HE.onMouseMove $ ChangeValueMouseMove
  ]

renderRecorderButton :: forall cs m. State -> H.ComponentHTML Action cs m
renderRecorderButton state =
  case state.recorderState of
    ReadyToRecord ->
      HH.span
      [ HP.class_ (wrap "recorder recorder-ready-to-record")
      , HP.title "Ready to record. Move the slider to start. Click to cancel"
      , HE.onClick $ const CancelRecording
      ]
      [ HH.text "⏺" ]
    Recording _ _ ->
      HH.span
      [ HP.class_ (wrap "recorder recorder-recording")
      , HP.title "Recording now. Move the slider" ]
      [ HH.text "⏺" ]
    Inactive ->
      case state.playbackState of
        PlaybackPaused (Just _) ->
          HH.span
          [ HP.class_ (wrap "recorder")
          , HP.title "Clear recording"
          , HE.onClick $ const DeleteRecording
          ]
          [ HH.text "🗑" ]
        _ ->
          HH.span
          [ HP.class_ (wrap "recorder recorder-start")
          , HP.title "Click to record"
          , HE.onClick $ const GetReadyToRecord
          ]
          [ HH.text "⏺" ]

renderPlaybackButton :: forall cs m. State -> H.ComponentHTML Action cs m
renderPlaybackButton state =
  case state.recorderState of
    ReadyToRecord -> HH.text ""
    Recording _ _ -> HH.text ""
    Inactive ->
      case state.playbackState of
        PlaybackPlaying _ _ ->
          HH.span
          [ HP.class_ (wrap "playback-button")
          , HP.title "Pause automation playback"
          , HE.onClick $ const PlaybackPause ]
          [ HH.text "⏸" ]
        PlaybackPaused (Just _) ->
          HH.span
          [ HP.class_ (wrap "playback-button")
          , HP.title "Resume automation playback"
          , HE.onClick $ const PlaybackResume ]
          [ HH.text "🞂" ]
        PlaybackPaused Nothing ->
          HH.text ""
        PlaybackReady _ -> HH.text ""

handleAction
  :: forall m cs
  .  MonadAff m
  => Action -> H.HalogenM State Action cs Message m Unit
handleAction = case _ of
  Init -> do
    void $ H.gets _.lastSentValue >>= raiseValue FromPlayback
    H.gets _.playbackState >>= case _ of
      PlaybackReady recording -> startPlaybackProcess recording
      _ -> pure unit
  GetReadyToRecord -> do
    H.modify_ \state ->
      state { recorderState = ReadyToRecord }
  CancelRecording ->
    H.modify_ \state ->
      state { recorderState = Inactive }
  MouseDown mouseEvent -> do
    state <- H.get
    case state.recorderState, state.playbackState of
      ReadyToRecord, _ -> do
        stopPlaybackProcess
        startTime <- liftEffect now
        value <- getValueFromEvent mouseEvent
        H.modify_
          _ { recorderState =
                 Recording startTime (List.singleton { relTime: wrap 0.0, value: value })
            , playbackState = PlaybackPaused Nothing
            }
        void $ raiseValue FromUser value
      Inactive, PlaybackPaused _ -> do
        when (shiftKey mouseEvent || ctrlKey mouseEvent) do
          stopPlaybackProcess
          H.put $ state
            { constantValue = state.defaultValue }
          void $ raiseValue FromUser state.defaultValue
      _, _ -> do
        pure unit
  StopRecordingAndSave mouseEvent -> do
    recorderState <- H.gets _.recorderState
    case recorderState of
      Recording lastTime entries -> do
        value <- getValueFromEvent mouseEvent
        void $ raiseValue FromPlayback value
        nowTime <- liftEffect now
        let
          newEntry = mkEntry nowTime lastTime value
          recordedEntries = Array.reverse $ Array.fromFoldable $ newEntry List.: entries
        startPlaybackProcess recordedEntries
        H.modify_ \state -> state { recorderState = Inactive }
        H.raise (UpdateRecording (Just recordedEntries))
      _ -> pure unit
  ChangeValue valueStr -> do
    from <- H.gets _.from
    to <- H.gets _.to
    for_ (valueFromString from to valueStr) \value -> do
      raiseValue FromUser value
  ChangeValueMouseMove mouseEvent -> do
    value <- getValueFromEvent mouseEvent
    nowTime <- liftEffect now
    state <- H.get
    shouldUpdate <- raiseValue FromUser value
    case state.recorderState of
      Recording lastTime entries -> do
        when shouldUpdate do
          H.modify_ _
            { recorderState =
                 let
                   newEntry = mkEntry nowTime lastTime value
                 in Recording nowTime (newEntry List.: entries)
            }
      _ -> do
        pure unit
  PlaybackPause -> do
    stopPlaybackProcess
  PlaybackResume -> do
    state <- H.get
    case state.playbackState of
      PlaybackPaused (Just recording) -> do
        startPlaybackProcess recording
      _ -> pure unit
  DeleteRecording -> do
    state <- H.get
    case state.playbackState of
      PlaybackPlaying _ _ -> do
        stopPlaybackProcess
      _ -> pure unit
    H.modify_ _ { playbackState = PlaybackPaused Nothing }
    H.raise (UpdateRecording Nothing)

mkEntry :: Instant -> Instant -> Value -> RecordingEntry
mkEntry nowTime lastTime value =
  { relTime:
    -- now - last
    unInstant nowTime <> negateDuration (unInstant lastTime)
  , value: value }

startPlaybackProcess
  :: forall m cs
  .  MonadAff m
  => Recording
  -> H.HalogenM State Action cs Message m Unit
startPlaybackProcess recording = do
  forkId <- H.fork do
    forever do
      for_ recording \entry -> do
        state <- H.get
        let
          delayTime = unwrap entry.relTime * state.playbackSpeed
          cappedDelay = if delayTime < 1.0 then 1.0 else delayTime
        liftAff $ delay $ wrap $ cappedDelay
        raiseValue FromPlayback entry.value
  H.modify_ \state ->
    state { playbackState = PlaybackPlaying recording forkId }

stopPlaybackProcess
  :: forall m cs
  .  MonadAff m
  => H.HalogenM State Action cs Message m Unit
stopPlaybackProcess = do
  state <- H.get
  case state.playbackState of
    PlaybackPlaying recording forkId -> do
      H.kill forkId
      H.put $ state
        { playbackState = PlaybackPaused (Just recording) }
    _ -> pure unit

-- | Tell the parent component a new value
raiseValue
  :: forall m cs
  .  MonadAff m
  => ValueUpdateType
  -> Value
  -> H.HalogenM State Action cs Message m Boolean
raiseValue updateType rawValue = do
  { lastSentValue, lastAmplitude, amplitude } <- H.get
  let
    value = rawValue * amplitude / 100
  let
    shouldUpdate = lastSentValue * lastAmplitude / 100 /= value
  when shouldUpdate do -- TODO: add lastUpdateType
    H.modify_ _ { lastSentValue = rawValue, lastAmplitude = amplitude }
    H.raise (UpdateValue updateType value rawValue )
  pure shouldUpdate

handleQuery
  :: forall a slots m
  .  MonadAff m
  => Query a
  -> H.HalogenM State Action slots Message m (Maybe a)
handleQuery (SetValueFromPlayer value cb) = do
  -- Set value if `RecorderState` is `Inactive`, otherwise ignore (because automation is used).
  state <- H.get
  case state.playbackState of
    PlaybackPaused _ -> do
      H.modify_ _ { lastSentValue = value }
    _ -> pure unit
  let
    isPlaying = case state.playbackState of
      PlaybackPlaying _ _ -> true
      PlaybackPaused _ -> false
      PlaybackReady _ -> false
  pure $ Just (cb isPlaying)
handleQuery (PutAmplitude amplitude a) = do
  state <- H.modify _ { amplitude = amplitude }
  case state.playbackState of
    PlaybackPaused _ -> do
      H.gets _.lastSentValue >>=
        void <<< raiseValue FromUser
    _ -> pure unit
  pure $ Just a
handleQuery (SetPlaybackSpeed speed a) = do
  H.modify_ _ { playbackSpeed = 1.0 / speed }
  pure $ Just a
