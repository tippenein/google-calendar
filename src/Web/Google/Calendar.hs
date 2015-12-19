{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Google.Calendar where
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client

data Calendar = Calendar {
    description :: Maybe Text
  , etag        :: Text -- ETag
  , id          :: Text
  , kind        :: Text  -- ex. ("calendar#calendar").
  , location    :: Maybe Text -- Geographic location of the calendar as free-form text
  , summary     :: Text -- Title of the calendar.
  , timeZone    :: Text
  } deriving (Show, Eq, Generic, Ord)

type GoogleCalendarApi =
  -- Calendars
       CalendarsClear
  :<|> CalendarsDelete
  :<|> CalendarsGet
  :<|> CalendarsCreate
  :<|> CalendarsUpdate

type CalendarsClear =
  -- /calendars/calendarId/clear
     "calendar" :> "v3" :> "calendars"
  :> Capture "calendarId" Text
  :> "clear"
  :> Post '[JSON] ()

type CalendarsDelete =
  -- /calendars/calendarId/
     "calendar" :> "v3" :> "calendars"
  :> Capture "calendarId" Text
  :> Delete '[JSON] ()

type CalendarsGet =
  -- /calendars/calendarId/
     "calendar" :> "v3" :> "calendars"
  :> Capture "calendarId" Text
  :> Get '[JSON] Calendar

type CalendarsCreate =
  -- /calendars
     "calendar" :> "v3" :> "calendars"
  :> ReqBody '[JSON] Calendar
  :> Post '[JSON] Calendar

type CalendarsUpdate =
  -- /calendars/calendarId
     "calendar" :> "v3" :> "calendars"
  :> Capture "calendarId" Text
  :> ReqBody '[JSON] Calendar
  :> Patch '[JSON] Calendar

