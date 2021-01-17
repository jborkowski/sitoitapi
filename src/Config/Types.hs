-- | App Configuration Types

module Config.Types where

import Data.Text (Text)
import Data.ByteString (ByteString)

newtype DBConnectionString = DBConnectionString
  { libpgConnectionString :: ByteString } deriving (Show)

data AppConfig = AppConfig
  { jwtSecret :: Text
  , database  :: DBConnectionString
  , host      :: Text
  , port      :: Int
  } deriving (Show)
