{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Stability   : Experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import Todo.API                             ( API )
import Todo.App
import Todo.Config
import System.Environment
import Servant.Server
import Servant.Mock
import Data.Proxy
import System.Envy
import Network.Wai.Handler.Warp             ( run )
------------------------------------------------------------------------------
-- | Application Entry Point
main :: IO ()
main = do
  result <- decodeEnv
  let api = Proxy :: Proxy API
  case result of
    Left errMsg -> print errMsg
    Right config@Config{..} -> do
      args <- getArgs
      case args of
        [ "mock" ] -> do
          putStrLn $ "Running mock server on " ++ show port ++ "..."
          run 8000 (serve api $ mock api)
        _ -> do
          putStrLn $ "Running server on " ++ show port ++ "..."
          run port (app config)


