{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.App
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Todo.App
       ( TodoApp    (..)
       , Config     (..)
       , app
       ) where
------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Servant
import Network.Wai                 ( Application )
------------------------------------------------------------------------------
import Todo.Config                 ( Config(..)  )
import Todo.API
import Todo.Core
import Todo.Type.Error
------------------------------------------------------------------------------
-- | Application
app :: Config -> Application
app cfg = serve (Proxy :: Proxy API) server
  where
    server :: Server API
    server = enter todoToEither todoEndpoints

    todoToEither :: TodoApp :~> EitherT ServantErr IO 
    todoToEither = Nat $ flip bimapEitherT id errorToServantErr
                         . flip runReaderT cfg . runTodo

    errorToServantErr :: Error -> ServantErr
    errorToServantErr = const err500 -- TODO: fill out
 
    
