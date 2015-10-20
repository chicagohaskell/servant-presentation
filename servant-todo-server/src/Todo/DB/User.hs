{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Todo.DB.User where

import qualified Data.HashMap.Strict  as H
import qualified Web.JWT as JWT
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.STM
import Control.Concurrent.STM
import Control.Monad
import Test.QuickCheck.Arbitrary
import Data.Maybe
import Todo.Type.Todo
import Todo.Type.User
import Todo.Type.UUID
import Data.Time.Clock.POSIX
import Data.UUID.V4 as UUID
import Servant.Common.Text

newtype UserDB = UserDB {
    _users  :: H.HashMap UserName User
  } deriving (Show, Eq, Monoid)

createToken :: MonadIO m => UserId -> m JWT.JSON
createToken (UserId uid) = do
  let claim :: JWT.JWTClaimsSet
      claim = JWT.def {
      JWT.sub = JWT.stringOrURI (toText uid)
    }
  return $ JWT.encodeSigned JWT.HS256 (JWT.secret "secret") claim

getUser :: MonadIO m => LoginUser -> TVar UserDB -> m User
getUser lu@LoginUser{..} tvar = liftIO $ do
  UserDB db <- atomically $ readTVar tvar
  case H.lookup user db of
   Just u -> return u
   Nothing -> do
     u <- registerUser lu
     atomically $ modifyTVar tvar $ \(UserDB db) -> UserDB $ H.insert user u db
     return u

registerUser :: MonadIO m => LoginUser -> m User
registerUser LoginUser {..} = liftIO $ do
  let userName = user
      password = pass
  userId <- UserId <$> nextUUID
  token  <- AuthToken <$> createToken userId
  return User{..}

nextUUID :: MonadIO m => m TodoUUID
nextUUID = TodoUUID <$> liftIO UUID.nextRandom
