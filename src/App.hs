{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module App
  where

import           API                         (api, server)
import           Config.Types                (AppContext, AppM, runApp)
import           Control.Monad.Reader        (runReaderT)
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors
import           Servant                     (Application, Context, Handler (..), Proxy (..),
                                              hoistServerWithContext, serveWithContext)
import           Servant.Auth.Server         as SAS (CookieSettings, JWTSettings)

mkApp :: Context '[ SAS.CookieSettings, SAS.JWTSettings ]
      -> CookieSettings
      -> JWTSettings
      -> AppContext
      -> Application
mkApp cfg cs jwts appContext =
  serveWithContext api cfg $
    hoistServerWithContext api (Proxy :: Proxy '[ SAS.CookieSettings, SAS.JWTSettings ])
      (toHandler appContext) (server cs jwts)
  where
    toHandler :: AppContext -> AppM a -> Handler a
    toHandler ctx a = Handler $ runReaderT (runApp a) ctx


corsPolicy :: CorsResourcePolicy
corsPolicy =
  CorsResourcePolicy {
    corsOrigins = Nothing,
    corsMethods = methods,
    corsRequestHeaders = ["Content-Type"],
    corsExposedHeaders = Nothing,
    corsMaxAge = Nothing,
    corsVaryOrigin = True,
    corsRequireOrigin = False,
    corsIgnoreFailures = False
  }
  where
    methods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    cont = simpleContentTypes <> ["application/json"]

corsConfig :: Middleware
corsConfig = cors (const $ Just corsPolicy)
