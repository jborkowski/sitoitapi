{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | App Configuration Types

module Config.Types
  where

import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.Reader       (MonadIO, MonadReader, ReaderT)
import           Data.ByteString            (ByteString)
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai.Handler.Warp   (HostPreference)
import           Servant

newtype DBConnectionString = DBConnectionString
  { libpgConnectionString :: ByteString } deriving (Show)

data AppConfig = AppConfig
  { jwtSecret   :: Text
  , database    :: DBConnectionString
  , host        :: HostPreference
  , port        :: Int
  , environment :: Text
  } deriving (Show)

data AppContext = AppContext
  { connectionPool :: Pool Connection
  -- , jwtSettings    :: JWTSettings
  }


newtype AppM a = AppT
  { runApp :: ReaderT AppContext (ExceptT ServerError IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppContext
             , MonadError  ServerError
             , MonadIO
             )
