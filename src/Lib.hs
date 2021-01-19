module Lib
  where

import           Config.Types           (AppM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (ToJSON, encode)
import           Servant


-- | Customize a `ServerError` having `s` status with `b` body contents
withBody :: (ToJSON b, MonadIO m) => ServerError -> b -> AppM m a
withBody s b = throwError (s { errBody = encode b })
