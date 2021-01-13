{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Config.Config
  ( loadConfig
  ) where

import           Config.Types
import qualified Data.Configurator as C

loadConfig :: FilePath -> IO AppConfig
loadConfig path = do
  config <- C.load [C.Required path]
  libpgConnectionString <- C.require config "db-libpg-connection-string"
  let database = DBConnectionString{..}
  jwtSecret  <- C.require config "jwt-secret"
  host       <- C.require config "app-host"
  port       <- C.require config "app-port"
  pure AppConfig{..}
