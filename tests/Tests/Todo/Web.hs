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
import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.Proxy
import           Servant hiding (Post)
import           Servant.Client
import           Test.Hspec
import qualified Web.JWT as JWT
------------------------------------------------------------------------------
-- | Proxification
todoApi :: Proxy TodoAPI
todoApi = Proxy
------------------------------------------------------------------------------
-- | Sigs
todoDELETE :: TodoId  -> Maybe JWT.JSON -> EitherT ServantError IO ()
todoGET    :: PostId     -> Maybe TimeStamp -> Maybe Limit -> Maybe JWT.JSON
              -> EitherT ServantError IO [Todo]
todoPOST   :: NewTodo -> Maybe JWT.JSON -> EitherT ServantError IO Todo
------------------------------------------------------------------------------
-- | Client Handlers
todoPOST :<|> todoGET :<|> todoDELETE = client todoApi host
  where host = BaseUrl Http "localhost" 8000
------------------------------------------------------------------------------
-- | Plus One Tests
todoWebTests :: Config -> SpecWith ()
todoWebTests config = do
  describe "Todo web tests" $ do
   void $ runIO $ runApp config $ redis flushall
   Right (token, uid) <- runIO $ regUser config
   it "Should create / delete a todo" $ do
    np <- newPost
    Right Post { postId = pid } <-
      runEitherT $ postPOST np (Just token)
    let nc = newTodo pid
    kk <- runEitherT $ todoPOST nc (Just token)
    let Right res@Todo{..} = kk
    todoPostId `shouldBe` pid
    todoUid `shouldBe` uid
    todoDeleted `shouldBe` False
    cc <- runEitherT $ postGETTodoCount pid (Just token)
    cc `shouldBe` Right (TodoCount pid 1)
    Right [res2] <- runEitherT $ todoGET pid Nothing Nothing (Just token)
    res `shouldBe` res2
    void $ runEitherT $ todoDELETE todoId (Just token)
    Right [ Todo { todoDeleted = del' } ] <-
      runEitherT $ todoGET pid Nothing Nothing (Just token)
    del' `shouldBe` True
