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
import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8       as Char8
import           Data.Maybe
import           Data.Pool                        (Pool, putResource, takeResource, withResource)
import           Data.Text.Encoding               (decodeUtf8)
import           Database.PostgreSQL.Simple       (Connection, Only (..), Query, begin, commit, execute,
                                                   query, query_)
import           Database.PostgreSQL.Simple.SqlQQ
import           Lib                              (withBody)
import           Logger.Types
import           Servant
import           Servant.Auth
import           Servant.Auth.Server
import           User.Types

type Login =
  "login"
  :> ReqBody '[JSON] LoginRequest
  :> Post '[JSON] UserLoginResponse

type GetUser = "my" :> Get '[JSON] AuthenticatedUser

type UserAPI = Login -- :<|> GetUser

login :: (MonadIO m)
      => CookieSettings
      -> JWTSettings
      -> LoginRequest
      -> AppM m UserLoginResponse
login cookieSettings jwtSettings req@LoginRequest{..} = do
  conns <- asks connectionPool
  (conn, pool) <- liftIO $ takeResource conns
  (users :: [DBUser]) <- liftIO $ query conn getByEmailSQL (Only reqEmail)
  case listToMaybe users of
    Nothing -> do
      liftIO $ putResource pool conn
      err401 `withBody` UnauthorizedUser
    Just user ->
      case validate req user of
        False -> do
          liftIO $ putResource pool conn
          err401 `withBody` UnauthorizedUser
        True -> do
          liftIO $ putResource pool conn
          let authedUser = toAuthUser user

          eJWT <- liftIO $ makeJWT authedUser jwtSettings Nothing

          case eJWT of
            Left a     -> do
              err401 `withBody` UnauthorizedUser
            Right jwt -> do
              pure $ Logged (Char8.unpack jwt) authedUser

              -- xsrfCookie <- liftIO $ makeXsrfCookie cookieSettings
              -- let modifiedSessionCookie = sessionCookie { setCookieHttpOnly = False, setCookieSecure = False }
          --xsrfCookie <- liftIO $ makeXsrfCookie cookieSettings
          --return $ (addHeader modifiedSessionCookie . addHeader xsrfCookie) NoContent


getByEmailSQL :: Query
getByEmailSQL = [sql|
  SELECT email, first_name, last_name, password, is_admin, created_at
  FROM "user"
  WHERE email = ? AND archived_at IS NULL
|]
