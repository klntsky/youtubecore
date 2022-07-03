-- Компонент для управления embed плеером
module YTM.YouTubeEmbed where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Promise (Promise, toAffE)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import YTM.Types (VideoId, VideoTitle, Volume, Opacity)

type Slot = H.Slot Query Message

foreign import data YouTubePlayer :: Type

type ElementId = String

foreign import newYouTubePlayer :: ElementId -> VideoId -> Volume -> Effect YouTubePlayer

type State =
  { player :: Maybe YouTubePlayer
  , playerId :: PlayerId
  , bgTask :: Maybe H.ForkId
  , volume :: Volume
  , opacity :: Opacity
  , videoId :: VideoId
  }

foreign import loadVideoTitle_
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> VideoId
  -> Effect (Promise (Maybe VideoTitle))

foreign import destroyPlayer :: YouTubePlayer -> Effect Unit

foreign import pausePlayer :: YouTubePlayer -> Effect Unit

foreign import resumePlayer :: YouTubePlayer -> Effect Unit

foreign import setVolume :: YouTubePlayer -> Volume -> Effect Unit

foreign import getVolume :: YouTubePlayer -> Effect Volume

foreign import setOpacity :: ElementId -> Number -> Effect Unit

foreign import setSize :: YouTubePlayer -> Int -> Int -> Effect Unit

loadVideoTitle :: VideoId -> Aff VideoTitle
loadVideoTitle =
  map (fromMaybe "[EMBEDDING NOT ALLOWED]") <<< toAffE <<< loadVideoTitle_ Just Nothing

foreign import loadVideoById :: YouTubePlayer -> VideoId -> Effect Unit

-- На какие запросы снаружи мы отвечаем?
data Query a
  = SetVolume Volume a
  | StartVideo VideoId a
  | PauseVideo a
  | ResumeVideo a
  | KillPlayer a
  | UpdateOpacity Opacity a
  | UpdatePlayerSize Int Int a

-- Что мы отправляем наружу?
data Message = SetTitle String

data Action = CreateYouTubePlayer | DestroyPlayer

type PlayerId = Int

type EmbedInit =
  { videoId :: VideoId
  , volume :: Volume
  , opacity :: Opacity }

mkComponent
  :: forall m
  .  MonadAff m
  => PlayerId
  -> H.Component Query EmbedInit Message m
mkComponent playerId =
  H.mkComponent
  { initialState: initialState playerId
  , render: render playerId
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just CreateYouTubePlayer
    }
  }

handleQuery
  :: forall a slots m
  .  MonadAff m
  => Query a
  -> H.HalogenM State Action slots Message m (Maybe a)
handleQuery (SetVolume volume a) = do
  H.modify_ _ { volume = volume }
  updatePlayerVolume
  updatePlayerOpacity
  pure $ Just a
handleQuery (StartVideo videoId a) = do
  state <- H.get
  case state.player of
    Nothing -> createYouTubePlayer videoId
    Just player -> do
      liftEffect $ loadVideoById player videoId
      title <- liftAff $ loadVideoTitle videoId
      H.raise (SetTitle title)
  pure $ Just a
handleQuery (KillPlayer a) = do
  H.gets _.player >>= traverse_ (liftEffect <<< destroyPlayer)
  pure $ Just a
handleQuery (PauseVideo a) = do
  H.gets _.player >>= traverse_ (liftEffect <<< pausePlayer)
  pure $ Just a
handleQuery (ResumeVideo a) = do
  H.gets _.player >>= traverse_ (liftEffect <<< resumePlayer)
  pure $ Just a
handleQuery (UpdateOpacity opacity a) = do
  H.modify_ _ { opacity = opacity }
  updatePlayerOpacity
  pure $ Just a
handleQuery (UpdatePlayerSize width height a) = do
  H.gets _.player >>= traverse_ \player -> do
    liftEffect $ setSize player width height
  pure $ Just a

updatePlayerOpacity
  :: forall slots m
  .  MonadAff m
  => H.HalogenM State Action slots Message m Unit
updatePlayerOpacity = do
  { playerId, volume, opacity } <- H.get
  let
    realOpacity = Int.toNumber (volume * opacity) / 10000.0
  liftEffect $ setOpacity (playerIdToElementId playerId) realOpacity

updatePlayerVolume
  :: forall slots m
  .  MonadAff m
  => H.HalogenM State Action slots Message m Unit
updatePlayerVolume = do
  { volume } <- H.get
  H.gets _.player >>= traverse_ \player -> do
    liftEffect $ setVolume player volume

initialState :: PlayerId -> EmbedInit -> State
initialState playerId { videoId, volume, opacity } =
  { player: Nothing
  , bgTask: Nothing
  , volume
  , opacity
  , videoId
  , playerId
  }

render :: forall cs m. PlayerId -> State -> H.ComponentHTML Action cs m
render playerId _state =
  HH.div
  [ HP.id (playerIdToElementId playerId)
  , HP.ref (H.RefLabel $ playerIdToElementId playerId)
  ]
  []

handleAction
  :: forall m cs
  .  MonadAff m
  => Action -> H.HalogenM State Action cs Message m Unit
handleAction = case _ of
  CreateYouTubePlayer -> do
    H.gets _.videoId >>= createYouTubePlayer
  DestroyPlayer -> do
    H.gets _.player >>= traverse_ \player -> do
      liftEffect $ destroyPlayer player
    H.gets _.bgTask >>= traverse_ H.kill

createYouTubePlayer
  :: forall cs m
  .  MonadAff m
  => VideoId
  -> H.HalogenM State Action cs Message m Unit
createYouTubePlayer videoId = do
  { volume, playerId } <- H.get
  player <- createPlayerElement playerId videoId volume
  updatePlayerOpacity
  forkId <- H.fork do
    forever do
      liftAff $ delay $ wrap 500.0
      newVolume <- liftEffect $ getVolume player
      oldVolume <- H.gets (_.volume)
      when (oldVolume /= newVolume) do
        liftEffect $ setVolume player oldVolume
  H.modify_ \state -> state { player = Just player
                            , bgTask = Just forkId
                            }
  title <- liftAff $ loadVideoTitle videoId
  H.raise (SetTitle title)

createPlayerElement
  :: forall cs m
  .  MonadAff m
  => PlayerId
  -> VideoId
  -> Volume
  -> H.HalogenM State Action cs Message m YouTubePlayer
createPlayerElement playerId videoId volume =
  liftEffect $ newYouTubePlayer (playerIdToElementId playerId) videoId volume

playerIdToElementId :: PlayerId -> String
playerIdToElementId playerId = "youtube-" <> show playerId
