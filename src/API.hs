{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- | Application API aggregator

module API
  ( API
  , api
  , server
  ) where

import           Config.Types        (AppM)
import qualified Flashcard.API       as F (FlashcardAPI, GetFlashcard, GetFlashcards, create, getAll, getById)
import           Servant             (Proxy (..), ServerT, (:<|>) ((:<|>)), (:>))
import           Servant.Auth.Server as SAS
import qualified User.API            as U (LoginAPI, UserAPI, login, user)
import           User.Types          (AUser)

type API auth =
       (SAS.Auth auth AUser :> F.GetFlashcards)
  :<|> (SAS.Auth auth AUser :> U.UserAPI)
  :<|> U.LoginAPI
  :<|> (SAS.Auth auth AUser :> F.GetFlashcard)

server :: JWTSettings -> ServerT (API auth) (AppM IO)
server jwts = F.getAll :<|> U.user :<|> U.login jwts :<|> F.getById

api :: Proxy (API '[JWT])
api = Proxy
