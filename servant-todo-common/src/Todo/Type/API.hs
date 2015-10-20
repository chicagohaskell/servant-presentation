{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.API
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Todo.Type.API where

import Servant.API
import Todo.Type.Todo (TodoAPI)
import Todo.Type.User (UserAPI)

type API = UserAPI :<|> TodoAPI
