module YTM.Types
       ( InvalidVideoURL
       , Opacity
       , RecordingEntry
       , Recording
       , Route
       , VideoId(..)
       , VideoParams
       , VideoTitle
       , VideoURLInput
       , Volume
       )
where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)

type VideoURLInput = String

type InvalidVideoURL = String

newtype VideoId = VideoId String

derive instance Generic VideoId _

derive instance Newtype VideoId _

instance Show VideoId where
  show = genericShow

type RecordingEntry =
  { relTime :: Milliseconds
  -- ^ Time relative to `StartTime`
  , value :: Volume
  -- ^ Current value
  }

-- TODO: NonEmpty
type Recording = Array RecordingEntry

-- | `0-100` value for video volume
type Volume = Int

-- | Title that is loaded from YouTube API
type VideoTitle = String

-- | `0-400` value for video opacity coefficient.
-- | Opacity property is calculated as `Opacity * Volume / 10000`
type Opacity = Int

type VideoParams =
  { videoId :: VideoId
  , settings :: NonEmptyArray -- TODO: s/settings/controls/
    { volume :: Volume
    , recording :: Maybe Recording -- TODO: s/recording/automation/
    , amplitude :: Int
    }
  , opacity :: Opacity
  }

type Route = List VideoParams
