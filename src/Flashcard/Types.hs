{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Types for flashcards

module Flashcard.Types
  where

import           Data.Aeson                           (FromJSON (..), ToJSON (..), Value (..), encode, object,
                                                       (.:), (.=))
import           Data.Aeson.Types                     (prependFailure, typeMismatch)
import           Data.Text                            (Text)
import           Data.Time                            (LocalTime)
import           Data.UUID                            (UUID)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)


data Flashcard = Flashcard
  { fcId        :: UUID
  , fcQuestion  :: Text
  , fcAnswer    :: Text
  , fcCategory  :: Text
  , fcStyle     :: Text
  , fcCreatedAt :: LocalTime
  } deriving (Show, Eq, Ord)

instance FromRow Flashcard where
  fromRow = Flashcard <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Flashcard where
  toJSON Flashcard{..} = object
    [ "answer" .= fcAnswer
    , "id" .= fcId
    , "question" .= fcQuestion
    , "category" .= fcCategory
    , "style" .= fcStyle
    , "createdAt" .= fcCreatedAt
    ]

instance FromJSON Flashcard where
  parseJSON (Object v) = do
    fcAnswer <- v .: "answer"
    fcId <- v .: "id"
    fcQuestion <- v .: "question"
    fcCategory <- v .: "caregory"
    fcStyle <- v .: "style"
    fcCreatedAt <- v .: "createdAt"
    pure $ Flashcard{..}
  parseJSON invalid = do
    prependFailure "parsing Flashcard failed, "
      (typeMismatch "Object" invalid)

data FlashcardRequest = FlashcardRequest
  { requestAnswer   :: Text
  , requestQuestion :: Text
  , requestStyle    :: Text
  , requestCategory :: Text
  } deriving (Show, Eq, Ord)

instance FromJSON FlashcardRequest where
  parseJSON (Object v) = do
    requestAnswer <- v .: "answer"
    requestQuestion <- v .: "question"
    requestStyle <- v .: "style"
    requestCategory <- v .: "category"
    pure $ FlashcardRequest{..}
  parseJSON invalid = do
    prependFailure "parsing FlashcardRequest failed, "
      (typeMismatch "Object" invalid)

data FlashcardResponse a =
    Success a
  | FlashcardNotFound
  | FlashcardBadRequest

instance (ToJSON a) => ToJSON (FlashcardResponse a) where
  toJSON (Success a) = toJSON a
  toJSON FlashcardNotFound = object
    [ "message" .= ("Cannot find flashcard with provided uuid" :: Text) ]
  toJSON FlashcardBadRequest = object
    [ "message" .= ("Bad Request" :: Text) ]
