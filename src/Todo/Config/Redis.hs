{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
module Todo.Config.Redis where
------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.Except
import           Database.Redis
import           System.Envy
------------------------------------------------------------------------------
-- | RedisConfig
data RedisConfig = RedisConfig {
    redisCon     :: Connection
  , redisConInfo :: ConnectInfo
  } 
------------------------------------------------------------------------------
instance Show RedisConfig where
  show RedisConfig{..} = show redisConInfo
------------------------------------------------------------------------------
instance FromEnv RedisConfig where
  fromEnv = do
    redisConInfo <- ConnInfo
                      <$> envMaybe "REDIS_HOST" .!= "localhost"
                      <*> pure (PortNumber 6379)
                      <*> envMaybe "REDIS_AUTH" 
                      <*> envMaybe "REDIS_DB" .!= 0
                      <*> envMaybe "REDIS_MAXCONNS" .!= 200
                      <*> pure 30
    result <- liftIO $ try $ connect redisConInfo
    case result of
      Left (e :: SomeException) -> throwError (show e)
      Right redisCon -> do
        liftIO . print $ redisConInfo
        r <- liftIO (runRedis redisCon ping)
        case r of
         Left e -> throwError (show e)
         Right k -> do
           liftIO $ print k
           return RedisConfig{..}
