{-# LANGUAGE DeriveGeneric #-}
-- | Types for flashcards

module Flashcard.Types where

import Data.Aeson

import qualified Data.UUID as U
import qualified Data.Text as T
import qualified Data.Time as Time

import GHC.Generics

data Flashcard = Flashcard
  { fcId        :: U.UUID
  , fcQuestion  :: T.Text
  , fcAnswer    :: T.Text
  , fcStyle     :: T.Text
  , fcCreatedAt :: Time.UTCTime
  } deriving (Generic, Show)

instance FromJSON Flashcard
instance ToJSON Flashcard
