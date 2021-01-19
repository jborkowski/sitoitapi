{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flashcard.API where

import Servant
import Flashcard.Types
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, query, query_, Query, Only(..), execute, begin, commit)
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID)

type CreateFlashcard = ReqBody '[JSON] FlashcardRequest :> PostCreated '[JSON] Flashcard
type GetFlashcards = Get '[JSON] [Flashcard]
type GetFlashcard = Capture "flashcardid" UUID :> Get '[JSON] Flashcard
type UpdateFlashcard = Capture "flashcardid" UUID :> ReqBody '[JSON] FlashcardRequest :> PutAccepted '[JSON] Flashcard
type DeleteFlashcard = Capture "flashcardid" UUID :> DeleteNoContent

type FlashcardAPI = "flashcard" :> (CreateFlashcard :<|> GetFlashcards :<|> GetFlashcard :<|> UpdateFlashcard :<|> DeleteFlashcard)

flashcardAPI :: Proxy FlashcardAPI
flashcardAPI = Proxy

create :: Pool Connection -> FlashcardRequest -> Handler Flashcard
create conns FlashcardRequest{..} =
  liftIO . withResource conns $ \conn -> do
    [flashcard] <- query conn insertSQL (requestCategory, requestQuestion, requestAnswer, requestStyle)
    pure flashcard

getAll :: Pool Connection -> Handler [Flashcard]
getAll coons =
  liftIO . withResource coons $ \conn -> query_ conn getAllSQL

getById :: Pool Connection -> UUID -> Handler Flashcard
getById coons uuid =
  liftIO . withResource coons $ \conn -> do
  [flashcard] <- query conn getByIdSQL (Only uuid)
  pure flashcard

update :: Pool Connection -> UUID -> FlashcardRequest -> Handler Flashcard
update coons uuid FlashcardRequest{..} =
  liftIO . withResource coons $ \conn -> do
  begin conn
  n <- execute conn updateSQL (requestCategory, requestQuestion, requestAnswer, requestStyle, uuid)
  [flashcard] <- query conn getByIdSQL (Only uuid)
  commit conn
  pure flashcard
  -- Add Errors handling
  -- throwError err400 {errBody = "Incorrect flashcard id"}

delete :: Pool Connection -> UUID -> Handler NoContent
delete coons uuid =
  liftIO . withResource coons $ \conn -> do
  _ <- execute conn deleteSQL (Only uuid)
  pure NoContent

insertSQL :: Query
insertSQL = [sql|
  INSERT INTO flashcard (id, category, question, answer, style, created_at)
  VALUES (uuid_generate_v4(), ?, ?, ?, ?, now())
  RETURNING id, question, answer, category, style, created_at
|]

updateSQL :: Query
updateSQL = [sql|
  UPDATE flashcard
  SET category = ?, question = ?, answer = ?, style = ?
  WHERE id = ? AND archived_at IS NULL
|]

getAllSQL:: Query
getAllSQL = [sql|
  SELECT id, question, answer, category, style, created_at
  FROM flashcard
  WHERE archived_at IS NULL
|]

getByIdSQL :: Query
getByIdSQL = [sql|
  SELECT id, question, answer, category, style, created_at
  FROM flashcard
  WHERE id = ? AND archived_at IS NULL
|]

deleteSQL :: Query
deleteSQL = [sql|
  UPDATE flashcard
  SET archived_at = now()
  WHERE id = ? AND archived_at IS NULL
|]
