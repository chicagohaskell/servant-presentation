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
import           Data.Aeson
import           Servant.Server.Internal
import           Control.Monad.Reader
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Text    (Text)
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Network.Wai.Internal
import           Network.Wai
import           Network.HTTP.Types
import qualified Web.JWT as JWT
------------------------------------------------------------------------------
import           Todo.Core
import           Todo.Config
import           Todo.Type.Todo
import           Todo.Type.User
import           Todo.DB.User
------------------------------------------------------------------------------
-- | Comment API
type UserAPI = "user" :> ReqBody '[JSON] LoginUser :> Post '[JSON] User
           :<|> "user" :> "hi" :> Get '[JSON] Value
------------------------------------------------------------------------------
userAPI :: ServerT UserAPI TodoApp
userAPI = loginUser :<|> hi


loginUser user = getUser user =<< asks userdb

hi = return $ object ["ay" .= ("yo" :: Text) ]


