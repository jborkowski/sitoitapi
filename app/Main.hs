{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main
  where

import           App                      (mkApp)
import           Config.Config            (loadConfig)
import           Config.Types
import           Init
import           Network.Wai.Handler.Warp
import           Servant                  (Context (..))
import           Servant.Auth.Server      (defaultJWTSettings, generateKey)
import           System.IO                (hPutStrLn, stderr)


main :: IO ()
main = do
  putStrLn "Starting Si to IT - API"
  AppConfig{..} <- loadConfig "./application.config"
  initDB database
  pool <- initConnectionPool database
  jwtKey <- generateKey

  let settings =
        setPort port $
        setHost host $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on " ++ show host ++ " port " ++ show port))
        defaultSettings
  let jwtCfg = defaultJWTSettings jwtKey
      context = AppContext pool
      cfg = jwtCfg :. EmptyContext

  runSettings settings $ mkApp cfg jwtCfg context
