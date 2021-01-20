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
import           Servant.Auth.Server      (IsSecure (NotSecure), cookieIsSecure, defaultCookieSettings,
                                           defaultJWTSettings, generateKey)
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
                           ("listening on " ++ show host ++ " port " ++ show port)) $
        defaultSettings
  let jwtCfg = defaultJWTSettings jwtKey
      myJwtCfg = JwtConfig jwtSecret jwtExpiry
      context = AppContext pool myJwtCfg
      cookieCfg = if environment == "dev"
                  then defaultCookieSettings{cookieIsSecure=NotSecure}
                  else defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext

  runSettings settings $ mkApp cfg jwtCfg context
