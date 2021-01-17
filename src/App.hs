-- |

module App where

import API (API, api)
import Data.Pool (Pool)
import Servant (Server, serve, (:<|>) ((:<|>)))
import Database.PostgreSQL.Simple (Connection)
import Flashcard.API (create, getAll, getById, update, delete)
import Network.Wai.Handler.Warp (run)
import Data.Text (Text)

-- Add custom AppM
server :: Pool Connection -> Server API
server conns = create conns :<|> getAll conns :<|> getById conns :<|> update conns :<|> delete conns

runApp :: Pool Connection -> Int -> Text -> IO ()
runApp coons port host = run port (serve api $ server coons)
