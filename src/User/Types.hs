{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- |

module User.Types where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), object)
import Data.Aeson.Types (prependFailure, typeMismatch)
import GHC.Generics
import Database.PostgreSQL.Simple.FromField
import Servant.Auth as SA
import Servant.Auth.Server (ToJWT, FromJWT)
import Data.ByteString (ByteString)
import Data.Time (LocalTime)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Data.Text (Text)

data LoginRequest = LoginRequest
  { reqEmail    :: Text
  , reqPassword :: Text
  } deriving (Show)

instance FromJSON LoginRequest where
  parseJSON (Object v) = do
    reqEmail <- v .: "email"
    reqPassword <- v .: "password"
    pure $ LoginRequest{..}
  parseJSON invalid = do
    prependFailure "parsing Login Request failed, "
      (typeMismatch "Object" invalid)

data AuthenticatedUser = AUser
  { auEmail     :: Text
  , auFirstName :: Text
  , auLastName  :: Text
  , auIsAdmin   :: Bool
  } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data DBUser = DBUser
  { dbEmail     :: ByteString
  , dbFirstName :: ByteString
  , dbLastName  :: ByteString
  , dbPassword  :: ByteString
  , dbIsAdmin   :: Bool
  , dbCreatedAt :: LocalTime
  }

instance FromRow DBUser where
  fromRow = DBUser <$> field <*> field <*> field <*> field <*> field <*> field
