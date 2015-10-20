{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
module Todo.Type.User where
------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances
import           Data.Hashable
import           Servant.Common.Text
------------------------------------------------------------------------------
import           Todo.Type.UUID
------------------------------------------------------------------------------
newtype UserId = UserId TodoUUID
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq, Hashable, Arbitrary)
------------------------------------------------------------------------------
newtype UserName = UserName Text
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq, Hashable, Arbitrary)
------------------------------------------------------------------------------
newtype Password = Password Text
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq, Arbitrary)
------------------------------------------------------------------------------
newtype AuthToken = AuthToken Text
  deriving (Show, FromText, ToText, ToJSON, FromJSON, Eq, Arbitrary)
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
------------------------------------------------------------------------------
instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
