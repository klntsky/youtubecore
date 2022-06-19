module Util.URIHash where

import Prelude

import Effect (Effect)
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

foreign import removeHash :: Effect Unit

setHash :: String -> Effect Unit
setHash "" = removeHash
setHash hash = do
  window <- HTML.window
  location <- Window.location window
  Location.setHash hash location

getHash :: Effect String
getHash = do
  window <- HTML.window
  location <- Window.location window
  hash <- Location.hash location
  pure hash
