{-# LANGUAGE OverloadedStrings #-}

module Users.Controller where

import           Control.Lens
import qualified Data.Text    as T
import           Network.Wai  (Response)
import           Web.Fn

import           Ctxt
import           Users.Model
import           Users.View

userRoutes :: Ctxt -> IO (Maybe Response)
userRoutes ctxt =
  route ctxt [ (end ==> usersHandler)]

usersHandler :: Ctxt -> IO (Maybe Response)
usersHandler ctxt = do
  users <- getUsers (ctxt ^. pool)
  putStrLn (T.unpack $ T.intercalate "\n" (map (T.pack . show) users))
  renderWith ctxt ["users", "index"] (usersSplices users)
