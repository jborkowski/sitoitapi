{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
-- | Application API aggregator

module API
  ( API
  , api
  , server
  ) where

import           Config.Types        (AppM)
import qualified Flashcard.API       as F (CreateFlashcard, DeleteFlashcard, GetFlashcard, GetFlashcards,
                                           UpdateFlashcard, create, delete, getAll, getById, update)
import           Servant             (Proxy (..), ServerT, err401, (:<|>) (..), (:>))
import           Servant.Auth.Server as SAS
import qualified User.API            as U (LoginAPI, UserAPI, login, user)
import           User.Types          (AUser)

type API auths =
  (SAS.Auth auths AUser :> ProtectedAPI) :<|> UnprotectedAPI

api :: Proxy (API '[JWT])
api = Proxy

type UnprotectedAPI =
  F.GetFlashcards :<|> U.LoginAPI

type ProtectedAPI =
  F.GetFlashcard :<|> F.UpdateFlashcard :<|> F.DeleteFlashcard :<|> F.CreateFlashcard

-- unprotected :: JWTSettings -> AppM UnprotectedAPI
unprotected cs jwts = F.getAll :<|> U.login jwts

--protected :: SAS.AuthResult AUser -> UUID
--            -> AppM ProtectedAPI
protected (SAS.Authenticated user) =
   F.getById :<|> F.update :<|> F.delete :<|> F.create
protected _    = throwAll err401

server :: CookieSettings -> JWTSettings -> ServerT (API auths) AppM
server cs jwts = protected  :<|> unprotected cs jwts
