{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Todo.Type.UUID where

import           Data.Aeson
import           Data.Aeson.Types
import           Control.Monad.IO.Class ( MonadIO(..) )
import           System.Random
import           Servant.Common.Text
import           Test.QuickCheck
import qualified Data.UUID.Types as UUID
import           Data.UUID.Types (UUID)
import           Data.Hashable

newtype TodoUUID = TodoUUID UUID
  deriving (Show, Eq, Hashable)

instance ToJSON TodoUUID where
    toJSON = String . toText

instance FromJSON TodoUUID where
  parseJSON val@(String x) =
    case fromText x :: Maybe TodoUUID of
      Nothing    -> typeMismatch "UUID" val
      Just vuuid -> pure vuuid
  parseJSON x = typeMismatch "UUID" x

instance Arbitrary TodoUUID where
  arbitrary = do
    num <- choose (minBound, maxBound)
    let gen = mkStdGen num
        (uuid, _) = random gen
    return $ TodoUUID uuid

instance ToText TodoUUID where
  toText (TodoUUID x) = UUID.toText x

instance FromText TodoUUID where
  fromText x = TodoUUID <$> UUID.fromText x
