{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
module Todo.Type.User where
------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Text as T
import           Data.Text    (Text)
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Data.UUID (UUID)
import           Data.Hashable
import           Servant.Common.Text
------------------------------------------------------------------------------
import           Todo.Type.UUID
------------------------------------------------------------------------------
newtype UserId = UserId TodoUUID
  deriving (Show, FromText, ToText, Arbitrary, ToJSON, FromJSON, Eq, Hashable)
------------------------------------------------------------------------------
newtype UserName = UserName Text
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq, Hashable)
------------------------------------------------------------------------------
newtype Password = Password Text
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq)
------------------------------------------------------------------------------
newtype AuthToken = AuthToken Text
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq)
------------------------------------------------------------------------------
data LoginUser = LoginUser {
     user :: UserName
  ,  pass :: Password
  } deriving (Show, Eq, Generic)
------------------------------------------------------------------------------
instance ToJSON LoginUser
instance FromJSON LoginUser
------------------------------------------------------------------------------
data User = User {
    userName :: UserName
  , userId   :: UserId
  , token    :: AuthToken
  , password :: Password
  } deriving (Show, Eq, Generic)
------------------------------------------------------------------------------
instance ToJSON User
instance FromJSON User
