module Main where

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude
import Util.URIHash (getHash)
import YTM.App (component)
import YTM.YouTubeAPI (awaitYouTubeAPI)

main :: Effect Unit
main = HA.runHalogenAff do
  awaitYouTubeAPI
  body <- HA.awaitBody
  -- Load app state from URI hash
  hash <- liftEffect getHash
  runUI component hash body
