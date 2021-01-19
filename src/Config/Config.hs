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
  hostStr    <- C.require config "app-host"
  let host = fromString hostStr
  port       <- C.require config "app-port"
  pure AppConfig{..}
