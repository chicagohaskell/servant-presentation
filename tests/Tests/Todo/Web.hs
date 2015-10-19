{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : Test.Todo.Web
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Tests.Todo.Web ( todoWebTests ) where
------------------------------------------------------------------------------
import           Control.Monad.Trans.Either
import           Data.Either
import           Servant hiding (Post)
import           Servant.Client
import           Test.Hspec
import           Todo.Type.User
import           Todo.Type.Todo
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
   it "Should create a todo" $ do
      Right User { token = token } <- makeUser
      x <- runEitherT $ todoCreate token (NewTodo "walk dog")
      x `shouldSatisfy` isRight
      Right (TodoCount count) <- runEitherT $ todoCount token
      count `shouldBe` 1
      Right todos <- runEitherT $ todoGetAll token Nothing Nothing
      length todos `shouldBe` 1
   it "Should update a todo" $ do
      Right User { token = token } <- makeUser
      Right Todo { todoId = todoId } <- runEitherT $ todoCreate token (NewTodo "walk dog")
      Right (Just Todo { description = description }) <-
        runEitherT $ todoUpdate token todoId (NewTodo "eat")
      description `shouldBe` Description "eat"
   it "Should delete a todo" $ do
      Right User { token = token } <- makeUser
      Right Todo { todoId = todoId } <- runEitherT $ todoCreate token (NewTodo "walk dog")
      Right () <- runEitherT $ todoDelete token todoId 
      Right (TodoCount count) <- runEitherT $ todoCount token
      count `shouldBe` 2


   






