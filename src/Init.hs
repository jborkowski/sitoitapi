{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |

module Init
  ( initDB
  , initConnectionPool
  ) where

import Control.Exception (bracket)
import Data.Pool
import Database.PostgreSQL.Simple (Connection, Query, connectPostgreSQL, close, execute_, execute, Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import Config.Types (DBConnectionString(..))
import Authentication

initDB :: DBConnectionString -> IO ()
initDB (DBConnectionString connStr) =
  bracket (connectPostgreSQL connStr) close $ \conn -> do
  _ <- execute_ conn createFlashcardTable
  _ <- execute_ conn enableExtensionUUID
  _ <- execute_ conn createUserTable
  encryptedPass <- encryptPassword "admin"
  _ <- execute conn createAdminUser (Only encryptedPass)
  pure ()

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool (DBConnectionString connStr) =
  createPool (connectPostgreSQL connStr)
             close
             2  --stripes
             60 -- unused connections are kept open for a minute
             5  -- max. 10 connections open per stripe

createFlashcardTable :: Query
createFlashcardTable = [sql|
  CREATE TABLE IF NOT EXISTS "flashcard" (
    id          UUID PRIMARY KEY,
    category    TEXT NOT NULL,
    question    TEXT NOT NULL,
    answer      TEXT NOT NULL,
    style       TEXT NOT NULL,
    created_at  TIMESTAMP NOT NULL,
    archived_at TIMESTAMP
  )
|]

enableExtensionUUID :: Query
enableExtensionUUID = [sql|
  CREATE EXTENSION IF NOT EXISTS "uuid-ossp"
|]

createUserTable :: Query
createUserTable = [sql|
  CREATE TABLE IF NOT EXISTS "user" (
    email       TEXT PRIMARY KEY NOT NULL,
    first_name  TEXT NOT NULL,
    last_name   TEXT NOT NULL,
    password    TEXT NOT NULL,
    is_admin    BOOLEAN DEFAULT FALSE,
    created_at  TIMESTAMP DEFAULT NOW(),
    archived_at TIMESTAMP DEFAULT NULL
  )
|]

createAdminUser :: Query
createAdminUser = [sql|
  INSERT INTO "user" (email, first_name, last_name, password, is_admin)
  VALUES ('admin@pm.me', 'Admin', 'Temporaty', ?, True)
  ON CONFLICT DO NOTHING
|]
