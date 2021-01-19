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
import           Data.Maybe
import           Data.Pool                        (Pool, putResource, takeResource, withResource)
import           Database.PostgreSQL.Simple       (Connection, Only (..), Query, begin, commit, execute,
                                                   query, query_)
import           Database.PostgreSQL.Simple.SqlQQ
import           Lib                              (withBody)
import           Logger.Types
import           Servant                          (Get, Header, Headers, JSON, Post, ReqBody, err305, err401,
                                                   (:>))
import           Servant.Auth                     as SA
import           Servant.Auth.Server              as SAS
import           User.Types

type Login =
  "login"
  :> ReqBody '[JSON] LoginRequest
  :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserLoginResponse)

type GetUser = "my" :> Get '[JSON] AuthenticatedUser

type UserAPI = Login -- :<|> GetUser

login :: CookieSettings
      -> JWTSettings
      -> LoginRequest
      -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie ] UserLoginResponse)
login cookieSettings jwtSetting req@LoginRequest{..} = do
  conns <- asks connectionPool
  (conn, pool) <- liftIO $ takeResource conns
  (users :: [DBUser]) <- liftIO $ query conn getByEmailSQL (Only reqEmail)
  case listToMaybe users of
    Nothing -> do
      liftIO $ putResource pool conn
      err401 `withBody` Unauthorized
    Just user ->
      case validate req user of
        False ->
          err401 `withBody` Unauthorized
        True ->
          err305 `withBody` InvalidForm
   -- liftIO $ case listToMaybe users of
   --   Nothing -> do
   --     throwIO err401
   --   Just usr ->
   --    throwIO err300



--liftIO $ withResource conns $ \conn -> do
    --users <- query conn getByEmailSQL (Only reqEmail)
    --case listToMaybe users of
      --Nothing -> undefined -- throwError err401
      --Just usr ->
       -- usr

getByEmailSQL :: Query
getByEmailSQL = [sql|
  SELECT email, first_name, last_name, password, is_admin, created_at
  FROM "user"
  WHERE email = ? AND archived_at IS NULL
|]
