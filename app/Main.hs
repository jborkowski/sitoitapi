module Main where

import Config.Config (loadConfig)

main :: IO ()
main = do
  config <- loadConfig "./application.config"
  print config
