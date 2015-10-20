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
import Control.Monad.IO.Class      ( MonadIO     )
import Control.Monad.Reader        ( MonadReader, ReaderT, runReaderT   )
import Control.Monad.Trans.Either  ( EitherT, runEitherT )
------------------------------------------------------------------------------
import Todo.Config              ( Config(..)  )
import Todo.Type.Error          ( Error       )
------------------------------------------------------------------------------
-- | Core Todo Type
newtype TodoApp a = TodoApp {
    runTodo ::  ReaderT Config (EitherT Error IO) a
  } deriving ( MonadIO, MonadReader Config
             , Applicative, Monad, Functor, MonadError Error )
------------------------------------------------------------------------------
-- | runApp - helper for transformer stack evaluation
runApp :: Config -> TodoApp a -> IO (Either Error a)
runApp config = runEitherT . flip runReaderT config . runTodo


