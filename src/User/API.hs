{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |

module User.API
  ( LoginAPI
  , UserAPI
  , login
  , user
  ) where

import           Authentication                   (validate)
import           Config.Types                     (AppM, JwtConfig (..), connectionPool, jwtConfig)
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Pool                        (putResource, takeResource)
import           Data.Time                        (addUTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple       (Only (..), Query, query)
import           Database.PostgreSQL.Simple.SqlQQ
import           Lib                              (withBody)
import           Servant
import           Servant.Auth.Server              as SAS
import           User.Types

type LoginAPI =
  "login"
  :> ReqBody '[JSON] LoginRequest
  :> Post '[JSON] UserLoginResponse

type GetUser = "me" :> Get '[JSON] AUser

type UserAPI = GetUser

login :: (MonadIO m)
      => JWTSettings
      -> LoginRequest
      -> AppM m UserLoginResponse
login jwtSettings req@LoginRequest{..} = do
  conns <- asks connectionPool
  (JwtConfig _ expiryIn) <- asks jwtConfig
  (conn, pool) <- liftIO $ takeResource conns
  (users :: [DBUser]) <- liftIO $ query conn getByEmailSQL (Only reqEmail)
  case listToMaybe users of
    Nothing -> do
      liftIO $ putResource pool conn
      err401 `withBody` UserNotFound
    Just dbuser ->
      case validate req dbuser of
        False -> do
          liftIO $ putResource pool conn
          err401 `withBody` InvalidCredentials
        True -> do
          liftIO $ putResource pool conn
          let authedUser = toAuthUser dbuser
          now <- liftIO getCurrentTime
          let expiry = addUTCTime expiryIn now
          eJWT <- liftIO $ SAS.makeJWT authedUser jwtSettings (Just expiry)

          case eJWT of
            Left _     -> do
              err401 `withBody` InvalidCredentials
            Right jwt -> do
              pure $ LoginSuccessful jwt authedUser

user :: (MonadIO m)
     => SAS.AuthResult AUser
     -> AppM m AUser
user (SAS.Authenticated auser) =
  pure auser
user _ = throwError err401

getByEmailSQL :: Query
getByEmailSQL = [sql|
  SELECT email, first_name, last_name, password, is_admin, created_at
  FROM "user"
  WHERE email = ? AND archived_at IS NULL
|]
