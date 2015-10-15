{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.Core
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Todo.Core ( TodoApp (..), runApp ) where
------------------------------------------------------------------------------
import Control.Monad.Except        ( MonadError  )
import Control.Monad.Trans.Control ( MonadBaseControl (..), StM )
import Control.Monad.IO.Class      ( MonadIO     )
import Control.Monad.Base          ( MonadBase   )
import Control.Monad.Reader        ( MonadReader, ReaderT, runReaderT   )
import Control.Monad.Trans.Either  ( EitherT, runEitherT )
------------------------------------------------------------------------------
import Todo.Config              ( Config(..)  )
import Todo.Type.Error          ( Error       )
------------------------------------------------------------------------------
-- | Core Todo Type
newtype TodoApp a = TodoApp {
    runTodo ::  ReaderT Config (EitherT Error IO) a
  } deriving ( MonadIO, MonadBase IO, MonadReader Config
             , Applicative, Monad, Functor, MonadError Error )
------------------------------------------------------------------------------
-- | Associated type for maintaining base context while forking
-- threads
newtype StMTodoApp a = StMTodoApp {
    runStMTodo :: StM (ReaderT Config (EitherT Error IO)) a
  }
------------------------------------------------------------------------------
-- | This allows us to fork a new thread while maintaining base context
instance MonadBaseControl IO TodoApp where
  type StM TodoApp a = StMTodoApp a
  liftBaseWith f = TodoApp
                 $ liftBaseWith
                 $ \g' -> f
                 $ \m -> fmap StMTodoApp
                 $ g' $ runTodo m
  restoreM = TodoApp . restoreM . runStMTodo
------------------------------------------------------------------------------
-- | runApp - helper for transformer stack evaluation
runApp :: Config -> TodoApp a -> IO (Either Error a)
runApp config = runEitherT . flip runReaderT config . runTodo


