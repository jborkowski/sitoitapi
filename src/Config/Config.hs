{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |

module Config.Config
  ( loadConfig
  ) where

import           Config.Types
import qualified Data.Configurator as C
import           Data.String       (fromString)

loadConfig :: FilePath -> IO AppConfig
loadConfig path = do
  config <- C.load [C.Required path]
  libpgConnectionString <- C.require config "db-libpg-connection-string"
  let database = DBConnectionString{..}
  jwtSecret  <- C.require config "jwt-secret"
  jwtExpiry  <- fromInteger <$> C.require config "jwt-expiry"
  host       <- fromString <$> C.require config "app-host"
  port       <- C.require config "app-port"
  environment<- C.require config "app-env"
  pure AppConfig{..}
