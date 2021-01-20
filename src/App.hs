{-# LANGUAGE DataKinds #-}
-- |

module App
  where

import           API                  (api, server)
import           Config.Types         (AppContext, AppM, runApp)
import           Control.Monad.Reader (runReaderT)
import           Servant              (Application, Context, Handler (..), Proxy (..), hoistServerWithContext,
                                       serveWithContext)
import           Servant.Auth.Server  as SAS (CookieSettings, JWTSettings)

mkApp :: Context '[ SAS.CookieSettings, SAS.JWTSettings ]
      -> JWTSettings
      -> AppContext
      -> Application
mkApp cfg jwts appContext =
  serveWithContext api cfg $
    hoistServerWithContext api (Proxy :: Proxy '[ SAS.CookieSettings, SAS.JWTSettings ])
      (toHandler appContext) (server jwts)
  where
    toHandler :: AppContext -> AppM IO a -> Handler a
    toHandler ctx a = Handler $ runReaderT (runApp a) ctx
