module Utils.FullScreen where

import Prelude
import Effect (Effect)

foreign import listenForFullscreenExit :: Effect Unit -> Effect Unit
