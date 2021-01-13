module Main where

import Config.Config (loadConfig)
import Config.Types (database)
import Init

main :: IO ()
main = do
  config <- loadConfig "./application.config"
  initDB (database config)
  print config
