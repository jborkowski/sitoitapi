{-# LANGUAGE RecordWildCards #-}
module Authentication
  ( encryptPassword
  , validate)
  where

import           Crypto.BCrypt         (fastBcryptHashingPolicy, hashPasswordUsingPolicy, validatePassword)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromMaybe)
import qualified Data.Text.Encoding    as E
import           User.Types            (DBUser (..), LoginRequest (..))

encryptPassword :: B.ByteString -> IO B.ByteString
encryptPassword password =
   fromMaybe (B.pack "")
     <$> hashPasswordUsingPolicy fastBcryptHashingPolicy password

validate :: LoginRequest -> DBUser -> Bool
validate LoginRequest{..} DBUser{..} =
  validatePassword dbPassword (E.encodeUtf8 reqPassword)
