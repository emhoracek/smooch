{-# LANGUAGE OverloadedStrings #-}

module Users.Controller where

import           Control.Lens
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Network.Wai  (Response)
import           Web.Fn

import           Ctxt
import           Users.Model

userRoutes :: Ctxt -> IO (Maybe Response)
userRoutes ctxt =
  route ctxt [ (end ==> usersHandler)]

usersHandler :: Ctxt -> IO (Maybe Response)
usersHandler ctxt = do
  users <- getUsers (ctxt ^. pool)
  okText $ T.pack $ show users
