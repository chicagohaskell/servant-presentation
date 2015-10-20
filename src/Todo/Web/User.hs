{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.Web.User
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Todo.Web.User
    ( -- * User API
      UserAPI
     -- * User API
    , userAPI
    ) where
------------------------------------------------------------------------------
import           Servant
import           Control.Monad.Reader
------------------------------------------------------------------------------
import           Todo.Core
import           Todo.Config
import           Todo.Type.User
import           Todo.DB.User
------------------------------------------------------------------------------
-- | Comment API
type UserAPI = "user" :> ReqBody '[JSON] LoginUser :> Post '[JSON] User
------------------------------------------------------------------------------
userAPI :: ServerT UserAPI TodoApp
userAPI user = getUser user =<< asks userdb



