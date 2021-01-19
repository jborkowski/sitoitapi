{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module User.API
  ( UserAPI
  , login) where

import Servant
import User.Types
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, query, query_, Query, Only(..), execute, begin, commit)
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad.IO.Class (liftIO)
import Authentication (validate)
-- import Control.Monad.Except
import Control.Exception (throwIO)
import           Control.Monad.Except             (throwError)

import Data.Maybe

type Login = "login" :> ReqBody '[JSON] LoginRequest :> PostCreated '[JSON] AuthenticatedUser
type GetUser = "my" :> Get '[JSON] AuthenticatedUser

type UserAPI = Login -- :<|> GetUser

login :: Pool Connection -> LoginRequest -> Handler AuthenticatedUser
login conns req@LoginRequest{..} =
  liftIO . withResource conns $ \conn -> do
  users <- query conn getByEmailSQL (Only reqEmail)
  case listToMaybe users of
    Nothing -> error "Cannot find user"
      -- throwError err404
    Just user ->
      case validate req user of
        True ->
          error "OK"
        False ->
          error "NOK"


getByEmailSQL :: Query
getByEmailSQL = [sql|
  SELECT email, first_name, last_name, password, is_admin, created_at
  FROM "user"
  WHERE email = ? AND archived_at IS NULL
|]
