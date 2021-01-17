-- | Application API aggregator

module API (API, api) where

import Flashcard.API (FlashcardAPI)
import Servant (Proxy(..))

type API = FlashcardAPI

api :: Proxy API
api = Proxy
