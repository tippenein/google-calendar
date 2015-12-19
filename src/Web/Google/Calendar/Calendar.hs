{-# LANGUAGE DeriveGeneric #-}

module Web.Google.Calendar.Calendar where
import Data.Text (Text)
import GHC.Generics

data Calendar = Calendar {
    description :: Maybe Text
  , etag        :: Text -- ETag
  , id          :: Text
  , kind        :: Text  -- ex. ("calendar#calendar").
  , location    :: Maybe Text -- Geographic location of the calendar as free-form text
  , summary     :: Text -- Title of the calendar.
  , timeZone    :: Text
  } deriving (Show, Eq, Generic, Ord)
