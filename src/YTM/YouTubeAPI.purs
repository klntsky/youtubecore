module YTM.YouTubeAPI
       (awaitYouTubeAPI)
where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude

awaitYouTubeAPI :: Aff Unit
awaitYouTubeAPI = toAffE loadYouTubeAPI_

foreign import loadYouTubeAPI_ :: Effect (Promise Unit)
