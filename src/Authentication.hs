{-# LANGUAGE RecordWildCards #-}
module Authentication where

import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy, validatePassword)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import User.Types (DBUser(..), LoginRequest(..))
import qualified Data.Text.Encoding as E

encryptPassword :: B.ByteString -> IO B.ByteString
encryptPassword password =
   fromMaybe (B.pack "")
     <$> hashPasswordUsingPolicy fastBcryptHashingPolicy password

validate :: LoginRequest -> DBUser -> Bool
validate LoginRequest{..} DBUser{..} =
  validatePassword dbPassword (E.encodeUtf8 reqPassword)
