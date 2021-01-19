{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config.Config (loadConfig)
import Config.Types
import Init
import App (mkApp)
import Network.Wai.Handler.Warp
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  putStrLn "Starting Si to IT - API"
  AppConfig{..} <- loadConfig "./application.config"
  initDB database
  pool <- initConnectionPool database
  let settings =
        setPort port $
        setHost host $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on " ++ show host ++ " port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp pool
