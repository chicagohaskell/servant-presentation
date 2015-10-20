{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Todo.DB.Todo where

import qualified Data.HashMap.Strict  as H
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.STM
import Test.QuickCheck.Arbitrary
import Data.Maybe
import Todo.Type.Todo
import Todo.Type.User
import Todo.Type.UUID
import Data.Time.Clock.POSIX

newtype TodoDB = TodoDB {
    _todos  :: H.HashMap UserId (H.HashMap TodoId Todo)
  } deriving (Show, Eq, Monoid, Arbitrary)

newTodoToTodo :: MonadIO m => NewTodo -> UserId -> m Todo
newTodoToTodo (NewTodo desc) userId = do
  created <- Created . round . (*1000) <$> liftIO getPOSIXTime -- millis
  completed <- pure (Completed False)
  description <- pure (Description desc)
  todoId <- TodoId <$> nextUUID
  return Todo{..}

addTodo :: MonadIO m => UserId -> Todo -> TVar TodoDB -> m Todo
addTodo uid todo tvar = liftIO $ do
  atomically $ modifyTVar tvar $ addTodo' uid todo
  return todo
  where
   addTodo' :: UserId -> Todo -> TodoDB -> TodoDB
   addTodo' uid todo@Todo{..} (TodoDB tododb) =
     TodoDB $ H.insertWith H.union uid (H.fromList [(todoId,todo)]) tododb 

getTodo :: MonadIO m => UserId -> TodoId -> TVar TodoDB -> m (Maybe Todo)
getTodo uid todoid tvar = do
  tododb <- liftIO $ atomically $ readTVar tvar
  return $ getTodo' uid todoid tododb 
   where
    getTodo' :: UserId -> TodoId -> TodoDB -> Maybe Todo
    getTodo' uid todoid (TodoDB tododb) = 
      H.lookup todoid =<< H.lookup uid tododb

getTodos :: MonadIO m => UserId -> TVar TodoDB -> m [Todo]
getTodos uid tvar = do
  tododb <- liftIO $ atomically $ readTVar tvar
  return $ getTodos' uid tododb
  where
    getTodos' :: UserId -> TodoDB -> [Todo]
    getTodos' uid (TodoDB tododb) =
      concat . maybeToList $ H.elems <$> H.lookup uid tododb

getTodoCount :: MonadIO m => UserId -> TVar TodoDB -> m TodoCount
getTodoCount uid tvar = do
  db <- liftIO $ atomically $ readTVar tvar
  return $ getTodoCount' uid db
   where
     getTodoCount' :: UserId -> TodoDB -> TodoCount
     getTodoCount' uid (TodoDB tododb) =
       fromMaybe (TodoCount 0) $
         TodoCount . fromIntegral . H.size <$> H.lookup uid tododb

deleteTodo :: MonadIO m => UserId -> TodoId -> TVar TodoDB -> m ()
deleteTodo uid todoid tvar = do
  liftIO $ atomically $ modifyTVar tvar $ deleteTodo' uid todoid
  where
    deleteTodo' uid todoid (TodoDB tododb) = do
      case H.delete todoid <$> H.lookup uid tododb of
        Nothing -> TodoDB tododb
        Just m  -> TodoDB (H.insert uid m tododb)

updateTodo :: MonadIO m => UserId -> TodoId -> NewTodo -> TVar TodoDB -> m (Maybe Todo)
updateTodo uid todoid (NewTodo desc) tvar = do
  todo <- getTodo uid todoid tvar
  case todo of
    Nothing -> return $ Nothing
    Just td -> Just <$> addTodo uid (td { description = Description desc }) tvar

defTodoDB :: TodoDB
defTodoDB = mempty
