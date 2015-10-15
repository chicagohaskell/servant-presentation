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
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Text    (Text)
import           Control.Monad.Except
import           Network.Wai.Internal
import qualified Web.JWT as JWT
------------------------------------------------------------------------------
import           Todo.Core
import           Todo.Type.Todo
import           Todo.DB.Todo
------------------------------------------------------------------------------
-- | Comment API
type TodoAPI =
       "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
       -- curl -XGET /todo/:id 
  :<|> "todo" :> "count" :> Get '[JSON] TodoCount
       -- curl -XGET /todo/ 
  :<|> "todo" :> QueryParam "orderby" OrderBy
              :> QueryParam "completed" Completed
              :> Get '[JSON] [Todo]
       -- curl -XGET /todo?desc?completed
  :<|> "todo" :> ReqBody '[JSON] NewTodo :> Post '[JSON] Todo
       -- curl -XPOST /todo/:id, -d { "body" : "walk dog" }
  :<|> "todo" :> Capture "id" TodoId  :> Delete '[JSON] ()
       -- curl -XDELETE /todo/:id
  :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] NewTodo :> Put '[JSON] Todo
       -- curl -XPUT /todo/:id, Body = { "body" : "walk cat" }, returning Todo
------------------------------------------------------------------------------
todoAPI :: ServerT TodoAPI TodoApp
todoAPI = todoGet
     :<|> todoCount
     :<|> todoGetAll
     :<|> todoCreate
     :<|> todoDelete
     :<|> todoUpdate
------------------------------------------------------------------------------
todoGet = undefined
todoCount = undefined
todoGetAll = undefined
todoCreate = undefined
todoDelete = undefined
todoUpdate = undefined

-- data JWT = JWT Text

-- instance HasServer api => HasServer (JWT :> api) where
--   type ServerT (JWT :> api) m = JWT -> ServerT api m
--   route Proxy subServer req@Request{..} resp =
--     case lookup "X-Access-Token" requestHeaders of
--       Nothing -> undefined
--       Just x  ->
--         route (Proxy :: Proxy api) (subServer (JWT (T.decodeUtf8 x))) req resp
