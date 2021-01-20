{-# LANGUAGE DataKinds #-}
-- | Application API aggregator

module API
  ( API
  , api
  , server
  ) where

import           Config.Types        (AppM)
import           Flashcard.API       (FlashcardAPI)
import           Servant             (Proxy (..), ServerT, (:<|>))
import           Servant.Auth.Server (JWT (..), JWTSettings)
import           User.API            (UserAPI, login)

--type API auths = UserAPI  -- :<|> FlashcardAPI
type API = UserAPI  -- :<|> FlashcardAPI

server :: JWTSettings -> ServerT (API ) (AppM IO)
server jwts = login jwts

api :: Proxy (API) --'[JWT])
api = Proxy
