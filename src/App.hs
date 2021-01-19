-- |

module App where

import API (API, api)
import Data.Pool (Pool)
import Servant (Server, serve, (:<|>) ((:<|>)), Application)
import Database.PostgreSQL.Simple (Connection)
import Flashcard.API (create, getAll, getById, update, delete)
import User.API (login)
import Data.Text (Text)

-- Add custom AppM
server :: Pool Connection -> Server API
server conns =
  login conns
  :<|> create conns
  :<|> getAll conns
  :<|> getById conns
  :<|> update conns
  :<|> delete conns

mkApp :: Pool Connection -> IO Application
mkApp connPool =
  pure $ serve api (server connPool)
