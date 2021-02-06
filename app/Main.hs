{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main
  where

import           App                      (mkApp, corsConfig)
import           Config.Config            (loadConfig)
import           Config.Types
import           Data.Maybe               (fromMaybe, listToMaybe)
import           Init
import           Network.Wai.Handler.Warp
import           Servant                  (Context (..))
import           Servant.Auth.Server      (IsSecure (NotSecure), cookieIsSecure, defaultCookieSettings,
                                           defaultJWTSettings, generateKey)
import           System.Environment       (getArgs)
import           System.IO                (hPutStrLn, stderr)


main :: IO ()
main = do
  args <- getArgs
  let configPath = fromMaybe "./application.config" (listToMaybe args)
  putStrLn "Starting Si to IT - API"
  AppConfig{..} <- loadConfig configPath
  initDB database
  pool <- initConnectionPool database
  jwtKey <- generateKey

  let settings =
        setPort port $
        setHost host $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on " ++ show host ++ " port " ++ show port)) $
        defaultSettings
  let jwtCfg = defaultJWTSettings jwtKey
      myJwtCfg = JwtConfig jwtSecret jwtExpiry
      context = AppContext pool myJwtCfg
      cookieCfg = if environment == "dev"
                  then defaultCookieSettings{cookieIsSecure=NotSecure}
                  else defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext

  runSettings settings $ corsConfig $ mkApp cfg cookieCfg jwtCfg context
