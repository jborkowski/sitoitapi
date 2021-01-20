{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- | Application API aggregator

module API
  ( API
  , api
  , server
  ) where

import           Config.Types        (AppM)
import           Flashcard.API       (FlashcardAPI)
import           Servant             (Proxy (..), ServerT, (:<|>) ((:<|>)), (:>))
import           Servant.Auth.Server (JWT (..), JWTSettings)
import           Servant.Auth.Server as SAS
import           User.API            (LoginAPI, UserAPI, login, user)
import           User.Types          (AUser)

type API auth = (SAS.Auth auth AUser :> UserAPI) :<|> LoginAPI
--type API auths = (FlashcardAPI auths) :<|> UserAPI

server :: JWTSettings -> ServerT (API auth) (AppM IO)
server jwts = user :<|> login jwts

api :: Proxy (API '[JWT])
api = Proxy
