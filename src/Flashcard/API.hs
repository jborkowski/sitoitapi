{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Flashcard.API
  ( CreateFlashcard
  , DeleteFlashcard
  , GetFlashcard
  , GetFlashcards
  , UpdateFlashcard
  , create
  , delete
  , getAll
  , getById
  , update
  ) where

import           Config.Types                     (AppM, connectionPool)
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Pool                        (withResource)
import           Data.UUID                        (UUID)
import           Database.PostgreSQL.Simple       (Only (..), Query, execute, query, query_)
import           Database.PostgreSQL.Simple.SqlQQ
import           Flashcard.Types
import           Servant

type CreateFlashcard = "flashcard" :> ReqBody '[JSON] FlashcardRequest :> PostCreated '[JSON] Flashcard
type GetFlashcards = "flashcard" :> Get '[JSON] [Flashcard]
type GetFlashcard = "flashcard" :> Capture "flashcardid" UUID :> Get '[JSON] (FlashcardResponse Flashcard)
type UpdateFlashcard = "flashcard" :> Capture "flashcardid" UUID :> ReqBody '[JSON] FlashcardRequest :> PutAccepted '[JSON] (FlashcardResponse Flashcard)
type DeleteFlashcard = "flashcard" :> Capture "flashcardid" UUID :> Delete '[JSON] NoContent

create :: FlashcardRequest
       -> AppM Flashcard
create FlashcardRequest{..} = do
  conns <- asks connectionPool
  liftIO . withResource conns $ \conn -> do
    [flashcard] <- query conn insertSQL (requestCategory, requestQuestion, requestAnswer, requestStyle)
    pure flashcard

getAll :: AppM [Flashcard]
getAll = do
  conns <- asks connectionPool
  liftIO . withResource conns $ \conn -> query_ conn getAllSQL

getById :: UUID
        -> AppM (FlashcardResponse Flashcard)
getById uuid = do
  conns <- asks connectionPool
  liftIO . withResource conns $ \conn -> do
    flashcards <- query conn getByIdSQL (Only uuid)
    pure $ maybe FlashcardNotFound Success (listToMaybe flashcards)

update :: UUID
       -> FlashcardRequest
       -> AppM (FlashcardResponse Flashcard)
update uuid FlashcardRequest{..} = do
  conns <- asks connectionPool
  liftIO . withResource conns $ \conn -> do
    n <- execute conn updateSQL (requestCategory, requestQuestion, requestAnswer, requestStyle, uuid)
    case n > 0 of
      True ->
        Success . head <$> query conn getByIdSQL (Only uuid)
      False ->
        pure FlashcardNotFound

delete :: UUID
       -> AppM NoContent
delete uuid = do
  conns <- asks connectionPool
  liftIO . withResource conns $ \conn -> do
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
