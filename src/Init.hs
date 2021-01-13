{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |

module Init
  ( initDB
  ) where

import Control.Exception (bracket)
import Data.Pool
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, execute_)
import Config.Types (DBConnectionString(..))
import Data.String.QQ

initDB :: DBConnectionString -> IO ()
initDB (DBConnectionString connStr) =
  bracket (connectPostgreSQL connStr) close $ \conn -> do
  _ <- execute_ conn createFlashcardTable
  pure ()

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool (DBConnectionString connStr) =
  createPool (connectPostgreSQL connStr)
             close
             2  --stripes
             60 -- unused connections are kept open for a minute
             5  -- max. 10 connections open per stripe

createFlashcardTable = [s|
  CREATE TABLE IF NOT EXISTS flashcard (
    id          UUID PRIMARY KEY,
    category    TEXT NOT NULL,
    question    TEXT NOT NULL,
    answer      TEXT NOT NULL,
    style       TEXT NOT NULL,
    created_at  TIMESTAMP NOT NULL,
    archived_at TIMESTAMP
  )
|]
