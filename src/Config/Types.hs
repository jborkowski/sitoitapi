{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | App Configuration Types

module Config.Types
  where

import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.Reader       (MonadIO, MonadReader, ReaderT)
import           Data.ByteString            (ByteString)
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import           Data.Time                  (NominalDiffTime)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai.Handler.Warp   (HostPreference)
import           Servant

newtype DBConnectionString = DBConnectionString
  { libpgConnectionString :: ByteString } deriving (Show)

data AppConfig = AppConfig
  { jwtSecret   :: Text
  , jwtExpiry   :: NominalDiffTime
  , database    :: DBConnectionString
  , host        :: HostPreference
  , port        :: Int
  , environment :: Text
  } deriving (Show)

data JwtConfig = JwtConfig
  { secret   :: Text
  , expiryIn :: NominalDiffTime
  }

data AppContext = AppContext
  { connectionPool :: Pool Connection
  , jwtConfig      :: JwtConfig
  }

newtype AppM a = AppM
  { runApp :: ReaderT AppContext (ExceptT ServerError IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppContext
             , MonadError ServerError
             , MonadIO
             )
