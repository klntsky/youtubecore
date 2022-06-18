module YTM.App where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens ((.~))
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (for_, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Util.URIHash (setHash)
import YTM.RecordableSlider as RecordableSlider
import YTM.Routing (parse) as Route
import YTM.Routing (stringify)
import YTM.Types (InvalidVideoURL, Recording, Route, VideoId(..), VideoURLInput, Volume)
import YTM.Types (Route) as Route
import YTM.VideoId (parseVideoId)
import YTM.VolumeControl as VolumeControl
import YTM.YouTubeEmbed as YouTubeEmbed

type State = { videos :: Map VideoIndex VideoState
             , lastId :: Int
             }

type VideoState = { videoId :: Either InvalidVideoURL VideoId
                  , settings :: NonEmptyArray
                    { volume :: Volume
                    , recording :: Maybe Recording
                    , amplitude :: Int
                    }
                  , title :: String
                  , deleted :: Boolean
                  }

data Action
  = AddVideo
  | RemoveVideo VideoIndex
  | HandleEmbedMessage VideoIndex YouTubeEmbed.Message
  | HandleVolumeControl VideoIndex VolumeControl.Message
  | UpdateVideoId VideoIndex VideoURLInput
  | StartVideos
  | PauseVideos
  | NoOp

data Query a
  = UpdateFromURIHash String a
  -- ^ Load state from router. Only happens on startup

-- index of a video in the State array
type VideoIndex = Int

type ChildSlots =
  ( embeds :: YouTubeEmbed.Slot Int
  , volumeControl :: VolumeControl.Slot Int
  )

component
  :: forall i o m
  .  MonadAff m
  => H.Component Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $
      H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }

initialState :: forall i. i -> State
initialState _ = { videos: Map.empty, lastId: 0 }

render
  :: forall m
  .  MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [ renderHeader
  , HH.div [ HP.id "flex-container" ]
    [ renderMixer state
    , renderVideos state
    ]
--  , HH.slot _volumeControl 0 VolumeControl.component 0 (const NoOp)
  ]

renderHeader :: forall m. H.ComponentHTML Action ChildSlots m
renderHeader = HH.div [ HP.id "header" ]
  [ HH.span [ HP.id "header-title" ]
    [ HH.text "YouTubeCore Player" ]
  , HH.span [ HP.id "player-controls" ]
    [ HH.span
      [ HP.id "play-button"
      , HP.class_ (wrap "player-button")
      , HE.onClick $ const StartVideos ]
      [ HH.text "PLAY" ]
    , HH.span
      [ HP.id "pause-button"
      , HP.class_ (wrap "player-button")
      , HE.onClick $ const PauseVideos ]
      [ HH.text "PAUSE" ]
    ]
  ]

renderVideos
  :: forall m
  .  MonadAff m
  => State -> H.ComponentHTML Action ChildSlots m
renderVideos state =
  HH.div [ HP.id "videos-container" ] $ map (uncurry renderVideo) $ Map.toUnfoldable state.videos

renderVideo
  :: forall m
  .  MonadAff m
  => Int -> VideoState -> H.ComponentHTML Action ChildSlots m
renderVideo idx {videoId, settings} =
  HH.div
  [ HP.ref (wrap $ show idx)
  , HP.class_ (wrap "video-container")
  , HP.style $ "opacity: " <> show (Int.toNumber 100 / 100.0)
  , HP.id ("tmp-" <> show idx)
  ]
  [ case videoId of
       Left _ -> HH.text ""
       Right validVideoId ->
         HH.slot _embeds idx
         (YouTubeEmbed.mkComponent idx (Just validVideoId) (NEA.head settings).volume)
         validVideoId
         (HandleEmbedMessage idx)
  ]

renderMixer
  :: forall m
  .  MonadAff m
  => State -> H.ComponentHTML Action ChildSlots m
renderMixer state = HH.div
  [ HP.id "mixer" ]
  [ HH.table_ $ Array.concat $
    Map.toUnfoldable state.videos <#> uncurry renderMixerEntry
  , HH.div [ HP.id "add-video-button-container" ]
    [ HH.span
      [ HE.onClick (\_ -> AddVideo)
      , HP.class_ (wrap "add-video-button")
      ]
      [ HH.text "ADD VIDEO" ]
    ]
  ]

renderMixerEntry
  :: forall m
  .  MonadAff m
  => Int
  -> VideoState
  -> Array (H.ComponentHTML Action ChildSlots m)
renderMixerEntry _idx { deleted: true } = []
renderMixerEntry idx videoState =
  [ HH.tr_
    [ HH.td
      [ HP.class_ (wrap "mixer-video-input-container") ]
      [ HH.input [ HP.type_ HP.InputText
                 , HP.class_ (wrap "mixer-video-input")
                 , HP.placeholder "Video URL or ID"
                 , HP.value $
                   case videoState.videoId of
                     Left videoId -> videoId
                     Right videoId -> unwrap videoId
                 , HE.onValueChange (UpdateVideoId idx)
                 ]
      , HH.span [ HE.onClick (\_ -> RemoveVideo idx)
                , HP.class_ (wrap "delete-button") ]
        [ HH.text "DELETE" ]
      ]
    , HH.td
      [ HP.rowSpan 2 ]
      [ HH.slot _volumeControl idx VolumeControl.component 0 (HandleVolumeControl idx)
      ]
    ]
  , HH.tr_
    [ HH.td
      [ HP.class_ (wrap "mixer-video-title") ]
      [ case videoState.videoId of
           Left _ -> HH.text ""
           Right videoId ->
             HH.a
             [ HP.href ("https://youtu.be/" <> unwrap videoId)
             , HP.target "_blank"
             , HP.class_ (wrap "video-link")
             ]
             [ HH.text shortenedTitle ]
      ]
    ]
  , HH.tr [ HP.class_ (wrap "mixer-entry-interval") ]
    [ HH.td [ HP.colSpan 2 ] [] ]
  ]
  where
    shortenedTitle =
      case CodeUnits.splitAt 50 videoState.title of
        { before, after }
          | after == "" -> before
          | otherwise -> before <> "…"

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of

  AddVideo -> do
    H.modify_ $ \state ->
      { videos: Map.insert
        state.lastId
        { videoId: Left ""
        , title: ""
        , deleted: false
        , settings: NEA.fromNonEmpty $ NonEmpty
          { recording: Nothing
          , volume: 0
          , amplitude: 100
          }
          [ { recording: Nothing
            , volume: 50
            , amplitude: 100
            }
          , { recording: Nothing
            , volume: 50
            , amplitude: 100
            }
          ]
        } state.videos
      , lastId: state.lastId + 1
      }

  RemoveVideo videoIndex -> do
    H.tell _embeds videoIndex YouTubeEmbed.KillPlayer
    liftAff $ delay $ wrap 1.0 -- TODO: is needed?
    H.modify_ \state -> state
      { videos = Map.update
           (\v -> pure $ v { deleted = true })
           videoIndex state.videos
      }
    updateURL

  -- ChangeVolume videoIndex newVolumeStr -> do
  --   changeVolume videoIndex newVolumeStr

  -- ChangeVolumeMouseMove videoIndex eventTarget -> do
  --   let
  --     mbInputElement =
  --       DOM.Element.fromEventTarget eventTarget >>= HTMLInputElement.fromElement
  --   case mbInputElement of
  --     Nothing -> liftEffect $ Console.log "Failed to update volume"
  --     _ -> pure unit
  --   for_ mbInputElement \input -> do
  --     liftEffect (HTMLInputElement.value input) >>= changeVolume videoIndex

  HandleEmbedMessage videoIndex msg ->
    case msg of

      YouTubeEmbed.UpdateVolume newVolume -> do
        pure unit
        -- isPlaying <- H.query _sliders videoIndex
        --   (H.mkRequest  (RecordableSlider.SetValueFromPlayer newVolume))
        -- H.modify_ \state -> state
        --   { videos =
        --        Map.update (Just <<< _ { volume = newVolume }) videoIndex state.videos }
        -- when (isPlaying == Just false) updateURL

      YouTubeEmbed.SetTitle newTitle -> do
        H.modify_ \state ->
          state { videos =
                     Map.update
                     (Just <<< _ { title = newTitle })
                     videoIndex
                     state.videos
                }

  HandleVolumeControl videoIndex msg -> do
    case msg of
      VolumeControl.UpdateValue updateType volume -> do
        void $ H.query _embeds videoIndex $
          H.mkTell (YouTubeEmbed.SetVolume volume)

        case updateType of
          RecordableSlider.FromUser -> do
            -- H.modify_ $ _videos <<< ix videoIndex <<< _settings <<< at 0 <<< _Just <<< _volume .~ volume
            -- updateURL
            pure unit

          RecordableSlider.FromPlayback -> do
            pure unit

      VolumeControl.UpdateRecordingStates states -> do
        H.modify_ $ _videos <<< ix videoIndex <<< _settings .~ states
        updateURL
      VolumeControl.UpdateOpacity opacity -> do
        void $ H.query _embeds videoIndex $ H.mkTell (YouTubeEmbed.UpdateOpacity opacity)

  UpdateVideoId videoIndex newVideoId -> do
    case parseVideoId newVideoId of
      Left invalidVideoUrl -> do
        H.modify_ \state ->
          state { videos =
                     Map.update (Just <<< _ { videoId = Left invalidVideoUrl })
                     videoIndex state.videos
                }
      Right videoId -> do
        H.modify_ \state ->
          state { videos =
                     Map.update (Just <<< _ { videoId = Right videoId })
                     videoIndex state.videos
                }
        _ <- H.query _embeds videoIndex $
             H.mkTell (YouTubeEmbed.StartVideo $ videoId )
        updateURL

  StartVideos -> do
    getExistingPlayerIds >>= traverse_
      (flip (H.tell _embeds) YouTubeEmbed.ResumeVideo)

  PauseVideos -> do
    getExistingPlayerIds >>= traverse_
      (flip (H.tell _embeds) YouTubeEmbed.PauseVideo)

  NoOp -> pure unit

updateURL :: forall m i
  .  MonadAff m
  => H.HalogenM State Action ChildSlots i m Unit
updateURL = do
  state <- H.get
  liftEffect $ Console.log $ "Updating URI hash"
  liftEffect $ setHash $ stringify $ getRoute state

getRoute :: State -> Route
getRoute { videos } = List.fromFoldable $ fold $
  Map.values videos <#> \video ->
  case video.videoId, video.deleted of
    Right validVideoId, false -> pure
      { videoId: validVideoId
      , settings: video.settings
      }
    _, _ -> []

getExistingPlayerIds
  :: forall m i
  .  MonadAff m
  => H.HalogenM State Action ChildSlots i m (Array Int)
getExistingPlayerIds =
  H.get <#> _.videos >>> Map.toUnfoldable >>> Array.mapMaybe
  \(videoIndex /\ videoState) ->
    if videoState.deleted then Nothing else Just videoIndex

handleQuery
  :: forall a m i
  .  MonadAff m
  => Query a
  -> H.HalogenM State Action ChildSlots i m (Maybe a)
handleQuery (UpdateFromURIHash hash a) = do
  let mbRoute = Route.parse hash
  liftEffect do
    Console.log $ "URI hash changed: " <> hash
    Console.log $ "Route: " <> show mbRoute
  case mbRoute of
    Nothing -> pure unit
    Just newRoute -> do
      -- kill all players
      indicesToKill <- getExistingPlayerIds
      for_ indicesToKill \indexToKill ->
        H.tell _embeds indexToKill YouTubeEmbed.KillPlayer
      -- Reset state
      H.modify_ (applyRoute newRoute)
      liftAff $ delay (wrap 1000.0)
      state <- H.get
      for_ (Map.toUnfoldable state.videos :: Array _)
        \(videoIndex /\ videoParams) -> do
          H.tell _volumeControl videoIndex
            (VolumeControl.PutRecordings videoParams.settings)
  pure $ Just a

applyRoute :: Route.Route -> State -> State
applyRoute route state =
  state { videos =
             Map.fromFoldable $ route `flip mapWithIndex` \idx videoParams ->
               (state.lastId + idx) /\
               { videoId: pure videoParams.videoId
               , title: ""
               , deleted: false
               , settings: videoParams.settings
               }
        , lastId = List.length route + state.lastId
        }

_videos = prop (Proxy :: Proxy "videos")
_volume = prop (Proxy :: Proxy "volume")
_settings = prop (Proxy :: Proxy "settings")

_embeds :: Proxy "embeds"
_embeds = Proxy

_sliders :: Proxy "sliders"
_sliders = Proxy

_volumeControl = Proxy :: Proxy "volumeControl"
