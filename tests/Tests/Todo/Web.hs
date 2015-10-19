{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : Test.Web.Todo
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Test.Web.Todo ( todoWebTests ) where
------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Either
import           Servant hiding (Post)
import           Servant.Client
import           Test.Hspec
import qualified Web.JWT as JWT
import           Todo.Type.User
import           Todo.Type.Todo
import           Todo.API
------------------------------------------------------------------------------
-- | Proxification
api :: Proxy API
api = Proxy
---------------------------------------------------------------
-- | Client Handlers
createUser
     :<|> todoGetAll
     :<|> todoGet
     :<|> todoDelete
     :<|> todoUpdate
     :<|> todoCount
     :<|> todoCreate = client api host
  where
    host = BaseUrl Http "localhost" 8000
------------------------------------------------------------------------------
-- | Plus One Tests
todoWebTests :: SpecWith ()
todoWebTests = do
   let makeUser = runEitherT $ createUser $ LoginUser (UserName "test") (Password "foo")
   it "Should create a user" $
     (`shouldSatisfy` isRight) =<< makeUser
   it "Should return 0 on initial todo count" $ do
      Right User{..} <- makeUser
      Right (TodoCount count) <- runEitherT $ todoCount token
      count `shouldBe` 0
   it "Should return an empty list with no todos" $ do
      Right User{..} <- makeUser
      Right todos <- runEitherT $ todoGetAll token Nothing Nothing
      todos `shouldBe` []

