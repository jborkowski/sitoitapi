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
  dbUsername <- C.require config "db.username"
  dbPassword <- C.require config "db.password"
  dbURI      <- C.require config "db.uri"
  let database = MkDatabaseConfig{..}
  jwtSecret  <- C.require config "jwt.secret"
  pure MkAppConfig{..}
