{-# LANGUAGE TypeOperators #-}
-- | Application API aggregator

module API (API, api) where

import Flashcard.API (FlashcardAPI)
import User.API (UserAPI)
import Servant (Proxy(..), (:<|>))

type API = UserAPI :<|> FlashcardAPI

api :: Proxy API
api = Proxy
