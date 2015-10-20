{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
------------------------------------------------------------------------------
module Todo.Type.Todo where
------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as T
-- import           Data.UUID (UUID)
-- import qualified Data.UUID as UUID
-- import qualified Data.UUID.V4 as UUID
import           GHC.Generics
import           Servant.Common.Text
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import           Todo.Type.UUID
import           Todo.Type.User
import           Servant.API
------------------------------------------------------------------------------
newtype Created = Created Integer
   deriving (Show, Eq, ToJSON, FromJSON, Arbitrary)

newtype Completed = Completed Bool
   deriving (Show, Eq, ToJSON, FromJSON, Arbitrary, FromText, ToText)

newtype Description = Description Text
   deriving (Show, Eq, ToJSON, FromJSON, Arbitrary)

newtype TodoCount = TodoCount Integer
   deriving (Show, Eq, ToJSON, FromJSON)

newtype NewTodo = NewTodo Text
   deriving (Show, Eq)

instance ToJSON NewTodo where
  toJSON (NewTodo todo) = object [ "todo" .=  todo ]

instance FromJSON NewTodo where
  parseJSON (Object o) = NewTodo <$> o .: "todo"

instance Arbitrary TodoCount where
  arbitrary = TodoCount . abs <$> arbitrary

data OrderBy = Asc | Desc deriving (Show, Eq)

instance ToText OrderBy where
  toText Asc  = "asc"
  toText Desc = "desc"

instance FromText OrderBy where
  fromText "asc"  = pure Asc
  fromText "desc" = pure Desc

newtype TodoId = TodoId TodoUUID
   deriving (Show, Eq, FromText, ToText, FromJSON, ToJSON, Arbitrary, Hashable)

data Todo = Todo {
    userId      :: UserId
  , todoId      :: TodoId
  , created     :: Created
  , completed   :: Completed
  , description :: Description
  } deriving (Show, Generic, Eq)

instance Arbitrary Todo where
  arbitrary =
    Todo <$> arbitrary <*> arbitrary 
         <*> arbitrary <*> arbitrary 
         <*> arbitrary

instance ToJSON Todo
instance FromJSON Todo

------------------------------------------------------------------------------
-- | Comment API
type TodoAPI =
       AuthToken :> "todo" :> QueryParam "orderby" OrderBy :> QueryParam "completed" Completed :> Get '[JSON] [Todo]
  :<|> AuthToken :> "todo" :> Capture "id" TodoId :> Get '[JSON] (Maybe Todo)
  :<|> AuthToken :> "todo" :> Capture "id" TodoId :> Delete '[JSON] ()
  :<|> AuthToken :> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] NewTodo :> Put '[JSON] (Maybe Todo)
  :<|> AuthToken :> "todo" :> "count" :> Get '[JSON] TodoCount
  :<|> AuthToken :> "todo" :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo
