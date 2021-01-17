{-# LANGUAGE RecordWildCards #-}
module Main where

import Config.Config (loadConfig)
import Config.Types
import Init
import App (runApp)

main :: IO ()
main = do
  AppConfig{..} <- loadConfig "./application.config"
  initDB database
  pool <- initConnectionPool database
  putStrLn ("Listening on " ++ show port)
  runApp pool port host
