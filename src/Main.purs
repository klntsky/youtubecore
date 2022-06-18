module Main where

import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude
import Util.URIHash (getHash)
import YTM.App (Query(..), component)
import YTM.YouTubeAPI (awaitYouTubeAPI)

main :: Effect Unit
main = HA.runHalogenAff do
  awaitYouTubeAPI
  body <- HA.awaitBody
  io <- runUI component unit body
  -- Load app state from URI hash
  hash <- liftEffect getHash
  void $ io.query $ UpdateFromURIHash hash unit
