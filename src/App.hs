-- |

module App
  where

import           API                        (API, api)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           Flashcard.API              (create, delete, getAll, getById, update)
import           Servant                    (Application, Server, serve, (:<|>) ((:<|>)))
import           User.API                   (login)

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
