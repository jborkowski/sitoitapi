{-# LANGUAGE DataKinds #-}
-- |

module App
  where

import           API                        (API, api, server)
import           Config.Types               (AppContext, AppM, runApp)
import           Control.Exception          (catch, throw)
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (runReaderT)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           Flashcard.API              (create, delete, getAll, getById, update)
import           Servant                    (Application, Context, Handler (..), Proxy (..), Server,
                                             hoistServerWithContext, serveWithContext, (:<|>) ((:<|>)))
import           Servant.Auth.Server        as SAS


-- Add custom AppM
  -- login conns
  -- :<|> create conns
  -- :<|> getAll conns
  -- :<|> getById conns
  -- :<|> update conns
  -- :<|> delete conns

mkApp :: Context '[SAS.CookieSettings, SAS.JWTSettings]
      -> CookieSettings
      -> JWTSettings
      -> AppContext
      -> Application
mkApp cfg cs jwts appContext =
  serveWithContext api cfg $
    hoistServerWithContext api (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
      (toHandler appContext) (server cs jwts)
  where
    toHandler :: AppContext -> AppM a -> Handler a
    toHandler ctx a = Handler $ runReaderT (runApp a) ctx
      --r <- liftIO $ runExceptT $ runReaderT (runApp a) appctx
      -- either throw pure r
