{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |

module User.API
  ( UserAPI
  , login
  ) where

import           Authentication                   (validate)
import           Config.Types                     (AppM, connectionPool)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Pool                        (Pool, putResource, takeResource, withResource)
import           Data.Text.Encoding               (decodeUtf8)
import           Database.PostgreSQL.Simple       (Only (..), Query, execute, query, query_)
import           Database.PostgreSQL.Simple.SqlQQ
import           Lib                              (withBody)
import           Servant
import           Servant.Auth.Server
import           User.Types

type Login =
  "login"
  :> ReqBody '[JSON] LoginRequest
  :> Post '[JSON] UserLoginResponse

type GetUser = "my" :> Get '[JSON] AuthenticatedUser

type UserAPI = Login -- :<|> GetUser

login :: (MonadIO m)
      => JWTSettings
      -> LoginRequest
      -> AppM m UserLoginResponse
login jwtSettings req@LoginRequest{..} = do
  conns <- asks connectionPool
  (conn, pool) <- liftIO $ takeResource conns
  (users :: [DBUser]) <- liftIO $ query conn getByEmailSQL (Only reqEmail)
  case listToMaybe users of
    Nothing -> do
      liftIO $ putResource pool conn
      err401 `withBody` UserNotFound
    Just user ->
      case validate req user of
        False -> do
          liftIO $ putResource pool conn
          err401 `withBody` InvalidCredentials
        True -> do
          liftIO $ putResource pool conn
          let authedUser = toAuthUser user

          eJWT <- liftIO $ makeJWT authedUser jwtSettings Nothing

          case eJWT of
            Left _     -> do
              err401 `withBody` InvalidCredentials
            Right jwt -> do
              pure $ LoginSuccessful jwt authedUser


getByEmailSQL :: Query
getByEmailSQL = [sql|
  SELECT email, first_name, last_name, password, is_admin, created_at
  FROM "user"
  WHERE email = ? AND archived_at IS NULL
|]
