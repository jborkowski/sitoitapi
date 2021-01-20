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
import qualified Data.ByteString.Lazy.Char8         as Char8
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Time                          (LocalTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
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
  } deriving (Show)

instance ToJSON AuthenticatedUser where
  toJSON AUser{..} = object
    [ "email" .= auEmail
    , "firstName" .= auFirstName
    , "lastName" .= auLastName
    , "isAdmin" .= auIsAdmin
    ]

instance FromJSON AuthenticatedUser where
  parseJSON (Object v) = do
    auEmail <- v .: "email"
    auFirstName <- v .: "firstName"
    auLastName <- v .: "lastName"
    auIsAdmin <- v .: "isAdmin"
    pure $ AUser{..}
  parseJSON invalid = do
    prependFailure "parsing AuthenticatedUser failed, "
      (typeMismatch "Object" invalid)

instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data DBUser = DBUser
  { dbEmail     :: Text
  , dbFirstName :: Text
  , dbLastName  :: Text
  , dbPassword  :: ByteString
  , dbIsAdmin   :: Bool
  , dbCreatedAt :: LocalTime
  }

toAuthUser :: DBUser -> AuthenticatedUser
toAuthUser (DBUser email fn ln _ isAdmin _) =
  AUser email fn ln isAdmin

instance FromRow DBUser where
  fromRow = DBUser <$> field <*> field <*> field <*> field <*> field <*> field

data UserLoginResponse
  = InvalidCredentials
  | UserNotFound
  | LoginSuccessful { ulrToken :: Char8.ByteString, ulrUser :: AuthenticatedUser }
  deriving (Show)

instance ToJSON UserLoginResponse where
  toJSON UserNotFound = object
    [ "message" .= ("Cannot found user with provided email address" :: Text) ]
  toJSON InvalidCredentials = object
    [ "message" .= ("Invalid credentials" :: Text) ]
  toJSON LoginSuccessful{..} = object
    [ "token" .= (decodeUtf8 . Char8.toStrict $ ulrToken)
    , "user" .= ulrUser
    ]
