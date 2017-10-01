{-# LANGUAGE OverloadedStrings #-}

module Users.View where

import qualified Data.Text   as T
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

dateSplice :: UTCTime -> Fill Ctxt
dateSplice = textFill . formatTimestamp

formatTimestamp :: UTCTime -> T.Text
formatTimestamp time =
  T.pack $ formatTime defaultTimeLocale "%B %e, %_Y" time
