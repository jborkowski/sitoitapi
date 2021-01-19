-- | App Configuration Types

module Config.Types
  where

import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (HostPreference)

newtype DBConnectionString = DBConnectionString
  { libpgConnectionString :: ByteString } deriving (Show)

data AppConfig = AppConfig
  { jwtSecret :: Text
  , database  :: DBConnectionString
  , host      :: HostPreference
  , port      :: Int
  } deriving (Show)
