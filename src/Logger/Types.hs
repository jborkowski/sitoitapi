{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Logger.Types
  where

import           Data.Aeson
import           Data.Text             (Text)
import           Data.Time             (UTCTime)
import           GHC.Generics
import           System.Log.FastLogger (ToLogStr (..))

data LogMessage = LogMessage
  { message      :: !Text
  , timestamp    :: !UTCTime
  , level        :: !Text
  , lversion     :: !Text
  , lenvironmnet :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode
