{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.API
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Todo.API ( TodoAPI, todoEndpoints, API ) where
------------------------------------------------------------------------------
import Servant
------------------------------------------------------------------------------
import Todo.Core
import Todo.Web.User
import Todo.Web.Todo
import Todo.Web.Docs
------------------------------------------------------------------------------
type API = UserAPI :<|> TodoAPI
------------------------------------------------------------------------------
todoEndpoints :: ServerT API TodoApp
todoEndpoints = userAPI :<|> todoAPI



