{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Web.Google.Calendar.Event where

import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics

import Web.Google.Calendar.User

data EventTime = EventTime {
  date     :: Day,
  dateTime :: UTCTime,
  timeZone :: Text
} deriving (Eq, Generic, Ord)

data Event = Event {
  kind                    :: Text, --"calendar#event",
  etag                    :: Text,
  eventId                 :: Text,
  status                  :: Text,
  htmlLink                :: Text,
  created                 :: UTCTime,
  updated                 :: UTCTime,
  summary                 :: Text,
  description             :: Text,
  location                :: Text,
  colorId                 :: Text,
  creator                 :: User,
  eventOrganizer          :: User,
  start                   :: EventTime,
  end                     :: EventTime,
  endTimeUnspecified      :: Bool,
  recurrence              :: [Text],
  recurringEventId        :: Text,
  originalStartTime       :: EventTime,
  transparency            :: Text,
  visibility              :: Text,
  iCalUID                 :: Text,
  sequence                :: Integer,
  attendees               :: [Attendant],
  attendeesOmitted        :: Bool,
  extendedProperties      :: ExtendedProperties,
  hangoutLink             :: Text,
  gadget                  :: Gadget,
  anyoneCanAddSelf        :: Bool,
  guestsCanInviteOthers   :: Bool,
  guestsCanModify         :: Bool,
  guestsCanSeeOtherGuests :: Bool,
  privateCopy             :: Bool,
  locked                  :: Bool,
  reminders               :: [Reminder],
  source                  :: Source,
  attachments             ::[Attachment]
  } deriving (Eq, Generic, Ord)

data Property = Property Text
  deriving (Show, Eq, Generic, Ord)
data ExtendedProperties = ExtendedProperties {
  private :: Property,
  shared  :: Property
} deriving (Show, Eq, Generic, Ord)

data Source = Source {
  url         :: Text,
  sourceTitle :: Text
} deriving (Show, Eq, Generic, Ord)

data Override = Override {
  method  :: Text,
  minutes :: Integer
} deriving (Show, Eq, Generic, Ord)

data Reminder = Reminder {
  useDefault :: Bool,
  overrides  :: [Override]
} deriving (Show, Eq, Generic, Ord)

data Attachment = Attachment {
  fileUrl            :: Text,
  attachmentTitle    :: Text,
  mimeType           :: Text,
  attachmentIconLink :: Text,
  fileId             :: Text
} deriving (Show, Eq, Generic, Ord)

data Preference = Preference {
  key :: Text  -- (key): string
} deriving (Show, Eq, Generic, Ord)

data Gadget = Gadget {
  gadgetType     :: Text,
  gadgetTitle    :: Text,
  link           :: Text,
  gadgetIconLink :: Text,
  width          :: Integer,
  height         :: Integer,
  display        :: Text,
  preferences    :: [Preference]
} deriving (Show, Eq, Generic, Ord)

data Attendant = Attendant {
  attendantId      :: Text,
  email            :: Text,
  displayName      :: Text,
  isOrganizer      :: Bool,
  self             :: Bool,
  resource         :: Bool,
  optional         :: Bool,
  responseStatus   :: Text,
  comment          :: Text,
  additionalGuests :: Integer
} deriving (Show, Eq, Generic, Ord)

