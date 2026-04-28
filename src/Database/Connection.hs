module Database.Connection
  ( withConnection,
  )
where

import App.Env (Application, connectionPool)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Connection)

withConnection :: (Connection -> IO a) -> Application a
withConnection action = do
  connectionPool <- asks connectionPool
  liftIO $ withResource connectionPool action
