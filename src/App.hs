{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |

module App
  where

import           API                  (API, api, server)
import           Config.Types         (AppContext, AppM, runApp)
import           Control.Monad.Reader (runReaderT)
import           Servant              (Application, Context, Handler (..), Proxy (..), hoistServerWithContext,
                                       serveWithContext)
import           Servant.Auth.Server  as SAS (CookieSettings, JWT, JWTSettings)

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
