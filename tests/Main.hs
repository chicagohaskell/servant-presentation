{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Main where
------------------------------------------------------------------------------
import GHC.Conc
import Test.Hspec
import Network.Wai.Handler.Warp ( run )
------------------------------------------------------------------------------
import Todo.App              ( app  )
------------------------------------------------------------------------------
import Todo.Test.Web
------------------------------------------------------------------------------
-- | Testing main
main :: IO ()
main = hspec spec
------------------------------------------------------------------------------
-- | Spec
spec :: Spec
spec = do
  config <- runIO $ do
    getConfig >>= \case
      Left err -> error (show err)
      Right c -> do
        _ <- forkIO $ run 8000 (app c)
        threadDelay 1000000 -- one second
        return c
  describe "Web Tests" $
    todoTests config
    
