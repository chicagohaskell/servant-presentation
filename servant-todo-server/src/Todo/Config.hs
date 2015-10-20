module Todo.Config where

import Control.Monad.IO.Class (liftIO)
import System.Envy
import Todo.DB.Todo
import Todo.DB.User
import Control.Concurrent.STM

data Config = Config {
  port  :: Int
, tododb :: TVar TodoDB
, userdb :: TVar UserDB
}

instance FromEnv Config where
  fromEnv = Config <$> envMaybe "TODO_PORT" .!= 8000
                   <*> (liftIO $ atomically (newTVar defTodoDB))
                   <*> (liftIO $ atomically (newTVar mempty))
