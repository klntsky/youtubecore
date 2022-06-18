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
import YTM.Types (VideoId, VideoTitle, Volume)
import YTM.VolumeControl (Opacity)

type Slot = H.Slot Query Message

foreign import data YouTubePlayer :: Type

type ElementId = String

foreign import newYouTubePlayer :: ElementId -> VideoId -> Volume -> Effect YouTubePlayer

type State = { player :: Maybe YouTubePlayer
             , bgTask :: Maybe H.ForkId
             , volume :: Volume
             , opacity :: Opacity
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

-- Что мы отправляем наружу?
data Message = SetTitle String | UpdateVolume Volume

data Action = CreateYouTubePlayer VideoId Volume | DestroyPlayer

type PlayerId = Int

mkComponent
  :: forall m
  .  MonadAff m
  => PlayerId -> Maybe VideoId -> Volume -> H.Component Query VideoId Message m
mkComponent playerId mbVideoId volume =
  H.mkComponent
  { initialState
  , render: render playerId
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction playerId
                                   , handleQuery = handleQuery playerId
                                   , initialize = mbVideoId <#> \videoId ->
                                     CreateYouTubePlayer videoId volume
                                   }
  }

handleQuery
  :: forall a slots m
  .  MonadAff m
  => PlayerId
  -> Query a
  -> H.HalogenM State Action slots Message m (Maybe a)
handleQuery playerId (SetVolume volume a) = do
  H.gets _.player >>= traverse_ \player ->
    liftEffect $ setVolume player volume
  setPlayerOpacity playerId volume
  pure $ Just a
handleQuery playerId (StartVideo videoId a) = do
  state <- H.get
  case state.player of
    Nothing -> createYouTubePlayer playerId videoId state.volume
    Just player -> do
      liftEffect $ loadVideoById player videoId
      title <- liftAff $ loadVideoTitle videoId
      H.raise (SetTitle title)
  pure $ Just a
handleQuery _ (KillPlayer a) = do
  H.gets _.player >>= traverse_ \player -> do
    liftEffect $ destroyPlayer player
  pure $ Just a
handleQuery _ (PauseVideo a) = do
  H.gets _.player >>= traverse_
    (liftEffect <<< pausePlayer)
  pure $ Just a
handleQuery _ (ResumeVideo a) = do
  H.gets _.player >>= traverse_
    (liftEffect <<< resumePlayer)
  pure $ Just a
handleQuery _ (UpdateOpacity opacity a) = do
  H.modify_ _ { opacity = opacity }
  pure $ Just a

setPlayerOpacity
  :: forall slots m
  .  MonadAff m
  => PlayerId
  -> Volume
  -> H.HalogenM State Action slots Message m Unit
setPlayerOpacity playerId volume = do
  opacity <- H.gets _.opacity
  liftEffect $ setOpacity (playerIdToElementId playerId)
    (Int.toNumber (volume * opacity) / 10000.0)

initialState :: forall i. i -> State
initialState _ = { player: Nothing, bgTask: Nothing, volume: 0, opacity: 100 }

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
  => PlayerId -> Action -> H.HalogenM State Action cs Message m Unit
handleAction playerId = case _ of
  CreateYouTubePlayer videoId volume -> do
    createYouTubePlayer playerId videoId volume
  DestroyPlayer -> do
    H.gets _.player >>= traverse_ \player -> do
      liftEffect $ destroyPlayer player
    H.gets _.bgTask >>= traverse_ H.kill

createYouTubePlayer playerId videoId volume = do
  player <- createPlayerElement playerId videoId volume
  setPlayerOpacity playerId volume
  forkId <- H.fork do
    forever do
      liftAff $ delay $ wrap 500.0
      newVolume <- liftEffect $ getVolume player
      oldVolume <- H.gets (_.volume)
      when (oldVolume /= newVolume) do
        H.raise (UpdateVolume newVolume)
        H.modify_ \state -> state { volume = newVolume }
  H.modify_ \state -> state { player = Just player
                            , bgTask = Just forkId
                            }
  title <- liftAff $ loadVideoTitle videoId
  H.raise (SetTitle title)

createPlayerElement playerId videoId volume =
  liftEffect $ newYouTubePlayer (playerIdToElementId playerId) videoId volume

playerIdToElementId :: PlayerId -> String
playerIdToElementId playerId = "youtube-" <> show playerId
