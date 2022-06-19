module YTM.Routing
       ( parse
       , stringify
       )
where

import Prelude

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (hush)
import Data.Foldable (fold, intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty (NonEmpty(..), fromNonEmpty)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (char, regex)
import Text.Parsing.StringParser.Combinators (many1, optionMaybe, sepBy)
import YTM.Types (Recording, RecordingEntry, Route, VideoParams, Volume, Opacity)
import YTM.VideoId (id)

stringify :: Route -> String
stringify route = intercalate "," (stringifyVideoParams <$> route)

stringifyVideoParams :: VideoParams -> String
stringifyVideoParams { videoId, settings, opacity } =
  unwrap videoId <>
  (
    fold $ settings <#> \entry ->
     "[" <>
     show entry.volume <> "*" <> show entry.amplitude <>
     (fromMaybe "" $ stringifyRecording <$> entry.recording) <>
     "]"
  ) <>
  "*" <> show opacity

stringifyRecording :: Recording -> String
stringifyRecording recording =
  "[" <> intercalate "," (stringifyRecordingEntry <$> recording) <> "]"

stringifyRecordingEntry :: RecordingEntry -> String
stringifyRecordingEntry { relTime, value } =
  show (Int.round (unwrap relTime)) <> "." <> show value

parse :: String -> Maybe Route
parse = hush <<< runParser routeParser

routeParser :: Parser Route
routeParser = do
  void $ char '#'
  try currentRouteParser <|> legacyRouteParser

currentRouteParser :: Parser Route
currentRouteParser = sepBy parseVideoParams (char ',')

-- | Route parser that only understands old URLs
legacyRouteParser :: Parser Route
legacyRouteParser = do
  sepBy legacyParseVideoParams (char ',')
  where
    legacyParseVideoParams :: Parser VideoParams
    legacyParseVideoParams = do
      videoId <- id
      void $ char '.'
      volume <- parseVolume
      pure { videoId
           , settings:
             NEA.fromNonEmpty $
             NonEmpty { volume, recording: Nothing, amplitude: 100 }
             [ { volume: 50, recording: Nothing, amplitude: 100 }
             , { volume: 50, recording: Nothing, amplitude: 100 }
             ]
           , opacity: 100
           }

parseVideoParams :: Parser VideoParams
parseVideoParams = do
  videoId <- id
  settings <- parseSettings
  opacity <- fromMaybe 100 <$> optionMaybe do
    void $ char '*'
    parseOpacity
  pure { videoId, settings, opacity }

parseSettings
  :: Parser (NonEmptyArray { volume :: Volume, recording :: Maybe Recording, amplitude :: Int })
parseSettings = do
  nonEmptyListToArray <$> many1 do
    void $ char '['
    volume <- parseVolume
    void $ char '*'
    amplitude <- parseAmplitude
    recording <- optionMaybe parseRecording
    void $ char ']'
    pure { volume, recording, amplitude }
  where
    nonEmptyListToArray = unwrap >>> fromNonEmpty
      \a -> NEA.fromNonEmpty <<< NonEmpty a <<< Array.fromFoldable

parseAmplitude :: Parser Int
parseAmplitude = parseInt

parseVolume :: Parser Volume
parseVolume = parseInt

parseOpacity :: Parser Opacity
parseOpacity = parseInt

parseInt :: Parser Int
parseInt = do
  mbInt <- Int.fromString <$> regex "[0-9]+"
  case mbInt of
    Nothing -> fail "Not an Int!"
    Just x -> pure x

parseRecording :: Parser Recording
parseRecording = do
  void $ char '['
  entries <- flip sepBy (char ',') do
    relTimeInt <- parseInt
    void $ char '.'
    value <- parseInt
    pure { relTime: wrap (Int.toNumber relTimeInt), value }
  void $ char ']'
  pure $ Array.fromFoldable entries
