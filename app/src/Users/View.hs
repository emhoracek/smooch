{-# LANGUAGE OverloadedStrings #-}

module Users.View where

import           Control.Monad.State (get, liftIO)
import qualified Data.Text           as T
import           Data.Time
import           Web.Larceny

import           Ctxt
import           Users.Model

usersSplices :: [User] -> Substitutions Ctxt
usersSplices users = subs
  [("users", mapSubs userSplices users)]

userSplices :: User -> Substitutions Ctxt
userSplices user = subs
  [("username", textFill (userUsername user))
  ,("email", textFill (userEmail user))
  ,("created-at", dateSplice (userCreatedAt user))
  ,("updated-at", dateSplice (userUpdatedAt user))]

loggedInUserSplices :: Substitutions Ctxt
loggedInUserSplices =
  subs [("loggedInUser", userFill)]
  where userFill = fillChildrenWith' $ do
          ctxt <- get
          mUser <- liftIO $ getLoggedInUser ctxt
          case mUser of
            Just user -> return $ userSplices user
            Nothing -> return mempty

dateSplice :: UTCTime -> Fill Ctxt
dateSplice = textFill . formatTimestamp

formatTimestamp :: UTCTime -> T.Text
formatTimestamp time =
  T.pack $ formatTime defaultTimeLocale "%B %e, %_Y" time

createUserErrorSplices :: Substitutions Ctxt
createUserErrorSplices =
          subs [ ("usernameErrors", textFill "")
               , ("emailErrors", textFill "")
               , ("passwordErrors", textFill "") ]