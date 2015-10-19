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
import Control.Monad
import System.Envy
import Test.Hspec
import Network.Wai.Handler.Warp ( run )
------------------------------------------------------------------------------
import Todo.App              ( app  )
import Todo.Config
------------------------------------------------------------------------------
import Tests.Todo.Web
------------------------------------------------------------------------------
-- | Testing main
main :: IO ()
main = hspec spec
------------------------------------------------------------------------------
-- | Spec
spec :: Spec
spec = do
  runIO $ do
    result <- decodeEnv
    case result of
       Left errMsg -> print errMsg
       Right config@Config{..} -> do
          void $ forkIO $ run 8000 (app config)
          threadDelay 1000000 -- one second
  describe "Web Tests" $
    todoWebTests

