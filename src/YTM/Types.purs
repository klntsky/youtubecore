module YTM.Types
       ( InvalidVideoURL
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

type Volume = Int

type VideoTitle = String

type VideoParams =
  { videoId :: VideoId
  , settings :: NonEmptyArray
    { volume :: Volume
    , recording :: Maybe Recording
    , amplitude :: Int
    }
  }

type Route = List VideoParams
