{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- | Application API aggregator

module API
  ( API
  , api
  , server
  ) where

import           Config.Types           (AppM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Flashcard.API          (FlashcardAPI)
import           Servant                (Proxy (..), ServerT, (:<|>))
import           Servant.Auth.Server    (CookieSettings, JWT (..), JWTSettings)
import           User.API               (UserAPI, login)

--type API auths = UserAPI  -- :<|> FlashcardAPI
type API = UserAPI  -- :<|> FlashcardAPI

server :: CookieSettings -> JWTSettings -> ServerT (API ) (AppM IO)
server cs jwts = login cs jwts

api :: Proxy (API) --'[JWT])
api = Proxy
