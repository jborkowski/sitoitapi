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
import Database.PostgreSQL.Simple (Connection, query, Query)
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad.IO.Class (liftIO)

type CreateFlashcard = ReqBody '[JSON] FlashcardRequest :> PostCreated '[JSON] Flashcard

type FlashcardAPI = "flashcard" :> CreateFlashcard

flashcardAPI :: Proxy FlashcardAPI
flashcardAPI = Proxy

create :: Pool Connection -> FlashcardRequest -> Handler Flashcard
create conns FlashcardRequest{..} =
  liftIO . withResource conns $ \conn -> do
    [flashcard] <- query conn insertSQL (requestCategory, requestQuestion, requestAnswer, requestStyle)
    pure flashcard

insertSQL :: Query
insertSQL = [sql|
  INSERT INTO flashcard (id, category, question, answer, style, created_at)
  VALUES (uuid_generate_v4(), ?, ?, ?, ?, now())
  RETURNING id, question, answer, category, style, created_at
|]

