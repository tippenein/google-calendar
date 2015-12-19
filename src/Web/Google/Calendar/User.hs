{-# LANGUAGE DeriveGeneric #-}

module Web.Google.Calendar.User where

import Data.Text (Text)
import GHC.Generics

data User = User {
  id          :: Text,
  email       :: Text,
  displayName :: Text,
  self        :: Bool
} deriving (Show, Eq, Generic, Ord)

