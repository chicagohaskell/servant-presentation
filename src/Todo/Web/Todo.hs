{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.Web.Todo
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Todo.Web.Todo
    ( -- * Todo API
      TodoAPI
     -- * Todo API
    , todoAPI
    ) where
------------------------------------------------------------------------------
import           Servant
import qualified Data.Text as T
import           Data.Text    (Text)
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
import           Servant.Client
import           Servant.Common.Req
import qualified Web.JWT as JWT
import           Servant.Common.Text
------------------------------------------------------------------------------
import           Todo.Core
import           Todo.Config
import           Todo.Type.Todo
import           Todo.Type.User
import           Todo.DB.Todo
------------------------------------------------------------------------------
-- | Comment API
type TodoAPI =
       AuthToken :> "todo" :> QueryParam "orderby" OrderBy :> QueryParam "completed" Completed :> Get '[JSON] [Todo]
  :<|> AuthToken :> "todo" :> Capture "id" TodoId :> Get '[JSON] (Maybe Todo)
  :<|> AuthToken :> "todo" :> Capture "id" TodoId :> Delete '[JSON] ()
  :<|> AuthToken :> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] NewTodo :> Put '[JSON] (Maybe Todo)
  :<|> AuthToken :> "todo" :> "count" :> Get '[JSON] TodoCount
  :<|> AuthToken :> "todo" :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo

------------------------------------------------------------------------------
todoAPI :: ServerT TodoAPI TodoApp
todoAPI = todoGetAll
     :<|> todoGet
     :<|> todoDelete
     :<|> todoUpdate
     :<|> todoCount
     :<|> todoCreate

------------------------------------------------------------------------------
todoGet :: UserId -> TodoId -> TodoApp (Maybe Todo)
todoGet uid todoId = getTodo uid todoId =<< asks tododb

todoCount :: UserId -> TodoApp TodoCount
todoCount uid = getTodoCount uid =<< asks tododb

todoGetAll :: UserId -> Maybe OrderBy -> Maybe Completed -> TodoApp [Todo]
todoGetAll uid _ _ = getTodos uid =<< asks tododb

todoCreate :: UserId -> NewTodo -> TodoApp Todo
todoCreate uid newtodo = do
  todo <- newTodoToTodo newtodo uid
  addTodo uid todo =<< asks tododb

todoDelete :: UserId -> TodoId -> TodoApp ()
todoDelete uid todoid = deleteTodo uid todoid =<< asks tododb

todoUpdate :: UserId -> TodoId -> NewTodo -> TodoApp (Maybe Todo)
todoUpdate uid todoid newtodo = updateTodo uid todoid newtodo =<< asks tododb

instance HasClient api => HasClient (AuthToken :> api) where
  type Client (AuthToken :> api) = AuthToken -> Client api
  clientWithRoute Proxy req url (AuthToken txt) =
    clientWithRoute (Proxy :: Proxy api) newreq url 
      where
        newreq = req { headers = [("X-Access-Token", txt)] ++ (headers req) }

instance HasServer api => HasServer (AuthToken :> api) where
  type ServerT (AuthToken :> api) m = UserId -> ServerT api m
  route Proxy subServer req@Request{..} resp =
    case getKey req of
      Nothing -> the401
      Just userid -> route (Proxy :: Proxy api) (subServer userid) req resp
   where
     the401 = resp . succeedWith $ responseLBS status401 [] "Invalid or missing Token"
     getKey :: Request -> Maybe UserId
     getKey Request{..} = do
       key <- lookup "X-Access-Token" requestHeaders
       sub <- JWT.sub . JWT.claims <$>
                 JWT.decodeAndVerifySignature (JWT.secret "secret") (T.decodeUtf8 key)
       fromText =<< JWT.stringOrURIToText <$> sub
