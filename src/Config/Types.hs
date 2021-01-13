-- | App Configuration Types

module Config.Types where

import qualified Data.Text as T
import Data.ByteString (ByteString)

newtype DBConnectionString = DBConnectionString
  { libpgConnectionString :: ByteString } deriving (Show)

data AppConfig = AppConfig
  { jwtSecret :: T.Text
  , database  :: DBConnectionString
  , host      :: T.Text
  , port      :: Int
  } deriving (Show)
