{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Env
  ( Env (..),
    Application,
  )
where

import App.Logger
  ( Logger,
    MonadLogger (askLogger),
  )
import Control.Monad.Reader (ReaderT, asks)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

data Env = Env
  { logger :: Logger,
    connectionPool :: Pool Connection
  }

type Application = ReaderT Env IO

instance MonadLogger Application where
  askLogger = asks logger
