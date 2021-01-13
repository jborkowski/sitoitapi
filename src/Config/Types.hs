-- | App Configuration Types

module Config.Types where

import qualified Data.Text as T

data DatabaseConfig = MkDatabaseConfig
  { dbUsername :: T.Text
  , dbPassword :: T.Text
  , dbURI      :: T.Text
  } deriving Show

data AppConfig = MkAppConfig
  { jwtSecret :: T.Text
  , database  :: DatabaseConfig
  } deriving Show
