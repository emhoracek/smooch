{-# LANGUAGE OverloadedStrings #-}

module Users.Controller where

import           Control.Lens
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Network.HTTP.Types (StdMethod (..))
import           Network.Wai        (Response)
import           Web.Fn

import           Ctxt
import           Users.Model
import           Users.View

userRoutes :: Ctxt -> IO (Maybe Response)
userRoutes ctxt =
  route ctxt [ (end ==> usersHandler)
             , (method POST // path "create"
                            // param "username"
                            // param "email"
                            // param "password"
                            // param "password-confirmation" !=> usersCreateHandler)]

usersHandler :: Ctxt -> IO (Maybe Response)
usersHandler ctxt = do
  users <- getUsers (ctxt ^. pool)
  putStrLn (T.unpack $ T.intercalate "\n" (map (T.pack . show) users))
  renderWith ctxt ["users", "index"] (usersSplices users)

usersCreateHandler :: Ctxt -> Text -> Text -> Text -> Text -> IO (Maybe Response)
usersCreateHandler ctxt username email password passwordConfirmation =
  if password == passwordConfirmation
  then
    do let newUser = NewUser username email password
       rows <- createUser (ctxt ^. pool) newUser
       if rows == 1
         then okHtml "created!"
         else errHtml "couldn't create user"
  else errHtml "Your passwords don't match"
