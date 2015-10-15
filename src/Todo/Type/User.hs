{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
module Todo.Type.User where
------------------------------------------------------------------------------
import           Data.Aeson
import           Test.QuickCheck.Arbitrary
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Data.UUID (UUID)
import           Servant.Common.Text
------------------------------------------------------------------------------
import           Todo.Type.UUID
------------------------------------------------------------------------------
newtype UserId = UserId TodoUUID
  deriving (Show, FromText, ToText, Arbitrary, ToJSON, FromJSON)

