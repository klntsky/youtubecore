module Main where

import Prelude

import Data.Int as Int
import Data.Maybe (maybe)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Util.URIHash (getHash)
import Web.DOM.Document (Document)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.ResizeObserver (observe, resizeObserver)
import YTM.App (Query(..), component)
import YTM.YouTubeAPI (awaitYouTubeAPI)

main :: Effect Unit
main = HA.runHalogenAff do
  awaitYouTubeAPI
  body <- HA.awaitBody
  -- Load app state from URI hash
  hash <- liftEffect getHash
  io <- runUI component hash body
  observer <- liftEffect $ resizeObserver \entries _ -> do
    for_ entries \entry -> do
      let
        width = Int.floor (entry.contentRect.width)
        height = Int.floor (entry.contentRect.height)
      launchAff_ $ io.query $ UpdateWindowSize width height unit
  document <- liftEffect getDocument
  htmlDocument <- liftEffect $
    HTMLDocument.fromDocument document # maybe (throw "no document") pure
  bd <- liftEffect $ HTMLDocument.body htmlDocument >>=
    maybe (throw $ "no document body") pure
  liftEffect $ observe (HTMLElement.toElement bd) {} observer

getDocument :: Effect Document
getDocument = HTML.window >>= map HTMLDocument.toDocument <<< Window.document
