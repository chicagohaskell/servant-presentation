module Todo.Config where

import System.Envy

data Config = Config {
  port :: Int
}

instance FromEnv Config where
  fromEnv = Config <$> envMaybe "TODO_PORT" .!= 8000
