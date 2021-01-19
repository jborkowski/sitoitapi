{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
-- |

module User.Types
  where

import           Data.Aeson                         (FromJSON (..), ToJSON (..), Value (..), object, (.:),
                                                     (.=))
import           Data.Aeson.Types                   (prependFailure, typeMismatch)
import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text)
import           Data.Time                          (LocalTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           GHC.Generics
import           Servant.Auth.Server                (FromJWT, ToJWT)


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

data UserLoginResponse
  = UserNotFound
  | Unauthorized
  | InvalidForm
  | Logged
  deriving (Show, Eq, Generic)

instance ToJSON UserLoginResponse
