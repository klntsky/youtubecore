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
import Effect (Effect)
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
import YTM.Types (InvalidVideoURL, Recording, Route, VideoId, VideoURLInput, Volume, Opacity)
import YTM.Types (Route) as Route
import YTM.VideoId (parseVideoId)
import YTM.VolumeControl (_opacity)
import YTM.VolumeControl as VolumeControl
import YTM.YouTubeEmbed as YouTubeEmbed

type State =
  { videos :: Map VideoIndex VideoState
  , lastId :: Int
  , fullscreen :: Boolean
  , size ::
       { width :: Int
       , height :: Int
       }
  }

type VideoState =
  { videoId :: Either InvalidVideoURL VideoId
  , settings :: NonEmptyArray
    { volume :: Volume
    , recording :: Maybe Recording
    , amplitude :: Int
    }
  , title :: String
  , deleted :: Boolean
  , opacity :: Opacity
  }

data Action
  = AddVideo
  | RemoveVideo VideoIndex
  | HandleEmbedMessage VideoIndex YouTubeEmbed.Message
  | HandleVolumeControl VideoIndex VolumeControl.Message
  | UpdateVideoId VideoIndex VideoURLInput
  | StartVideos
  | PauseVideos
  | FullScreen
  | NoOp

data Query a
  = UpdateFromURIHash String a
  -- ^ Load state from router. Only happens on startup
  | UpdateWindowSize Int Int a
  | ExitFullscreen a

-- index of a video in the State array
type VideoIndex = Int

type ChildSlots =
  ( embeds :: YouTubeEmbed.Slot Int
  , volumeControl :: VolumeControl.Slot Int
  )

type URIHash = String

component
  :: forall o m
  .  MonadAff m
  => H.Component Query URIHash o m
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

initialState :: URIHash -> State
initialState hash =
  case mbRoute of
    Nothing -> defaultState
    Just route -> applyRoute route defaultState
  where
    mbRoute = Route.parse hash
    defaultState =
      { videos: Map.empty
      , lastId: 0
      , fullscreen: false
      , size:
        { width: 1024
        , height: 768
        }
      }

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
  , HH.div [ HP.id "description" ]
    [ HH.h2_ [ HH.text "About" ]
    , HH.p_
      [ HH.text """
        YouTubeCore is a sound mixer and a VJ tool that uses YouTube embeded players.
        """
      ]
    , HH.p_
      [ HH.text """
        This project pushes the idea of playing multiple videos at once to the extreme.
        """
      ]
    , HH.p_
      [ HH.text """
        I use it as a way to experiment with sound.
        """
      ]

    , HH.h2_ [ HH.text "Getting started" ]
    , HH.p_
      [ HH.text """
        To start from zero, add a YouTube video above and move VOLume slider. Your setup ("patch") will be saved in the URL, so that you can share it online. You can use an example patch
        """
      , HH.a [ HP.href "https://youtubecore.sigil.network/#N0Epas3_JKQ[48*95[0.26,160.27,84.32,27.33,42.35,14.37,68.42,26.44,49.48,57.49]][54*100[0.51,123.52,99.53,66.54,387.55,170.56,69.58,248.59,118.60,146.61,107.62,41.63,103.64,38.65,143.66,43.67,273.68,335.69,107.70,144.71,266.72,272.73,71.74,768.75,464.71,36.70,60.69,82.67,366.66,121.65,515.64,242.63,657.62,154.61,145.60,301.59,66.58,120.57,181.56,619.55,637.54,452.53]][50*100]*0,sJU5Mj7OXnw[58*97][50*100][50*100]*400,1IL8sVw6Scs[61*97[0.19,119.20,17.21,55.23,19.30,32.31,32.37,27.44,19.48,36.49,25.56,45.57,32.61,94.63]][64*100][50*100]*400,5Y6iQz3vuM8[91*100[0.0,146.4,26.27,25.57,12.76,28.78,19.79,25.82,14.86,28.87,23.88,12.91,5.93]][38*100[0.50,531.52,410.53,89.55,67.56,220.57,97.58,151.59,150.60,201.61,171.62,22.63,51.69,525.70,37.69,28.68,68.67,25.66,107.65,99.64,50.63,334.62,133.61,67.60,84.59,117.58,99.57,51.56,34.55,166.54,35.53,50.52,165.51,102.50,60.49,73.47,50.46,25.45,42.44,65.43,33.42,503.41,116.39,199.38,450.37]][50*100]*29,goyZbut_KFY[44*100[0.0,171.2,28.3,49.6,15.17,47.21,38.33,28.41,30.44,57.44]][50*100][50*100]*16,v1oLtgZQwKg[50*100[0.48,66.50,76.47,16.46,18.45,34.44,51.40,34.33,25.15,24.4,19.3,31.0,44.0]][49*100][50*100]*0,evH__YKvU-8[33*97[0.46,114.49,13.48,67.44,26.37,28.33,29.31,48.28,30.22,42.21,68.18,94.17,44.20,28.48,34.53,22.58,42.60,51.71,118.75,162.71,19.65,14.63,23.50,15.49,30.48,34.43,33.37,59.33,450.32]][71*100][50*100]*0,ff-5c5JnVHY[25*100][50*100][50*100]*89", HP.target "_blank" ]
        [ HH.text "here" ]
      , HH.text
        """ (you will be prompted to allow media autoplay by the browser).
        """
      ]

    , HH.p_
      [ HH.text """
        Some of the controls have an automation recorder button. Press "record" and move the corresponding slider freely - your movement will start repeating indefinitely after you release the slider.
        """
      ]
    , HH.h2_ [ HH.text "Controls" ]
    , HH.p_
      [ HH.text """
        VOL - video volume, also connected with video player opacity
        """
      ]
    , HH.p_
      [ HH.text """
        SPD - playback speed of video volume automation. Has no effect if the automation is not set for VOL
        """
      ]
    , HH.p_
      [ HH.text """
        MOD - playback speed of SPD automation. Has no effect is the automation is not set for SPD
        """
      ]
    , HH.p_
      [ HH.text """
        OPC - coefficient that determines how much does current volume value influence video player opacity
        """
      ]
    , HH.p_
      [ HH.text """
        AMP - amplitude control of the slider to the left
        """
      ]
    , HH.br_

    , HH.h2_ [ HH.text "Links" ]

    , HH.a [ HP.href "https://t.me/youtubecore", HP.target "_blank" ]
      [ HH.text "TELEGRAM CHANNEL with reusable example patches" ]
    , HH.br_
    , HH.a [ HP.href "https://github.com/klntsky/youtubecore", HP.target "_blank" ]
      [ HH.text "SOURCE CODE" ]
    , HH.br_
    , HH.a [ HP.href "https://twitter.com/klntsky/", HP.target "_blank" ]
      [ HH.text "AUTHOR'S TWITTER" ]
    ]
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
      [ HH.text "PLAY" ] -- TODO: hide when no videos exist
    , HH.span
      [ HP.id "pause-button"
      , HP.class_ (wrap "player-button")
      , HE.onClick $ const PauseVideos ]
      [ HH.text "PAUSE" ]
    , HH.span
      [ HP.id "fullscreen-button"
      , HP.class_ (wrap "player-button")
      , HE.onClick $ const FullScreen ]
      [ HH.text "FULLSCREEN" ]
    ]
  ]

renderVideos
  :: forall m
  .  MonadAff m
  => State -> H.ComponentHTML Action ChildSlots m
renderVideos state =
  HH.div [ HP.id "videos-container" ] $
  map (uncurry (renderVideo state)) $
  Map.toUnfoldable state.videos

renderVideo
  :: forall m
  .  MonadAff m
  => State
  -> Int
  -> VideoState
  -> H.ComponentHTML Action ChildSlots m
renderVideo { size, fullscreen } idx { opacity, videoId, settings } =
  HH.div
  [ HP.ref (wrap $ show idx)
  , HP.class_ (wrap "video-container")
  , HP.style $ "opacity: " <> show (Int.toNumber 100 / 100.0)
  , HP.id ("tmp-" <> show idx)
  ]
  [ case videoId of
       Left _ -> HH.text ""
       Right validVideoId ->
         let thisVideo = NEA.head settings in
         HH.slot _embeds idx
         (YouTubeEmbed.mkComponent idx)
         { videoId: validVideoId
         , volume: thisVideo.volume * thisVideo.amplitude / 10000
         , opacity
         , size: toVideoSize fullscreen size
         }
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
renderMixerEntry idx videoState@{ settings, opacity } =
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
      [ HH.slot _volumeControl idx VolumeControl.component
        { settings, opacity }
        (HandleVolumeControl idx)
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
      state
      { videos = Map.insert
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
        , opacity: 100
        } state.videos
      , lastId = state.lastId + 1
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

  HandleEmbedMessage videoIndex (YouTubeEmbed.SetTitle newTitle) -> do
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

      VolumeControl.UpdateRecordingStates { settings, opacity } -> do
        H.modify_ $
          (_videos <<< ix videoIndex <<< _settings .~ settings) >>>
          (_videos <<< ix videoIndex <<< _opacity .~ opacity)
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

  FullScreen -> do
    liftEffect requestFullscreen
    { width, height } <- H.modify _ { fullscreen = true } <#> _.size
    updateWindowSize width height

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
      , opacity: video.opacity
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
      liftAff $ delay (wrap 1.0)
      state <- H.get
      for_ (Map.toUnfoldable state.videos :: Array _)
        \(videoIndex /\ videoParams) -> do
          H.tell _volumeControl videoIndex $ VolumeControl.PutRecordings
            { settings: videoParams.settings
            , opacity: videoParams.opacity }
  pure $ Just a
handleQuery (UpdateWindowSize width height a) = do
  updateWindowSize width height
  pure $ Just a
handleQuery (ExitFullscreen a) = do
  { width, height } <- H.modify _ { fullscreen = false } <#> _.size
  updateWindowSize width height
  pure $ Just a

updateWindowSize
  :: forall m i
  .  MonadAff m
  => Int
  -> Int
  -> H.HalogenM State Action ChildSlots i m Unit
updateWindowSize width height = do
  isFullscreen <- H.gets _.fullscreen
  let
    { videoWidth, videoHeight } = toVideoSize isFullscreen { width, height }
  H.modify_ _ { size = { width, height } }
  getExistingPlayerIds >>= traverse_ \ix -> do
    H.tell _embeds ix $ YouTubeEmbed.UpdatePlayerSize videoWidth videoHeight

toVideoSize :: Boolean -> { width :: Int, height :: Int } -> { videoWidth :: Int, videoHeight :: Int }
toVideoSize isFullscreen { width, height } =
  let
    minCap cap x
      | x < cap = cap
      | otherwise = x
    videoWidth /\ videoHeight =
        if isFullscreen
        then
           minCap 300 width /\ minCap 300 height
        else
           minCap 300 (width - 750) /\ minCap 300 (height - 50)
  in { videoWidth, videoHeight }


applyRoute :: Route.Route -> State -> State
applyRoute route state =
  state { videos =
             Map.fromFoldable $ route `flip mapWithIndex` \idx videoParams ->
               (state.lastId + idx) /\
               { videoId: pure videoParams.videoId
               , title: ""
               , deleted: false
               , settings: videoParams.settings
               , opacity: videoParams.opacity
               }
        , lastId = List.length route + state.lastId
        }

foreign import requestFullscreen :: Effect Unit

_videos = prop (Proxy :: Proxy "videos")
_volume = prop (Proxy :: Proxy "volume")
_settings = prop (Proxy :: Proxy "settings")

_embeds :: Proxy "embeds"
_embeds = Proxy

_sliders :: Proxy "sliders"
_sliders = Proxy

_volumeControl = Proxy :: Proxy "volumeControl"
