{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : Todo.Config.Auth
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Todo.Config.Auth where
------------------------------------------------------------------------------
import           System.Envy
import qualified Web.JWT as JWT
------------------------------------------------------------------------------
-- | Secret Key
newtype SecretKey = SecretKey JWT.Secret deriving (Show, Eq)
------------------------------------------------------------------------------
-- | FromEnv Secret Key
instance FromEnv SecretKey where
  fromEnv = return $ SecretKey $ JWT.secret "SuperSecretKey"
