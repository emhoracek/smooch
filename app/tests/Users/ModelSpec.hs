{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Users.ModelSpec where

import           Control.Monad       (void)
import           Control.Monad.Trans (liftIO)
import           System.Random
import           Test.Hspec
import           Test.Hspec.Fn

import           Common
import           Ctxt
import           Users.Model

instance Factory Ctxt User (HspecFn NewUser) where
  fields = do rand <- liftIO randomIO
              let username = "user" <> tshow rand
              let email = username <> "@test.com"
              let pass = "pass" <> tshow rand
              return $ NewUser username email pass
  save fNewUser = do newUser <- fNewUser
                     Just user <-
                       eval (\ctxt -> do
                                void $ createUser ctxt newUser
                                getUserByUsername ctxt (newUserUsername newUser))
                     return user

spec :: Spec
spec = fnTests $ do
  describe "getUsers" $ do
    it "should get all the users" $ do
      newUser <- create id
      users <- eval getUsers
      (newUser `elem` users) `shouldEqual` True
  describe "createUser" $ do
    it "should create a new user" $ do
      void $ eval (\ctxt -> createUser ctxt
                            (NewUser "new" "new@new.com" "pass"))
      [user] <- eval getUsers
      userEmail user `shouldEqual` "new@new.com"
  describe "getUserByUsername" $ do
    it "should get the user with that username" $ do
      newUser <- create id
      let username = userUsername newUser
      Just user <- eval (`getUserByUsername` username)
      userEmail user `shouldEqual` userEmail newUser
  describe "getUserByEmail" $ do
    it "should get the user with that email" $ do
      newUser <- create id
      let email = userEmail newUser
      Just user <- eval (`getUserByEmail` email)
      userUsername user `shouldEqual` userUsername newUser
  describe "authenticateUser" $ do
    it "should get Just the user with that email and password" $ do
      void $ eval (\ctxt -> createUser ctxt
                            (NewUser "new" "new@new.com" "pass"))
      Just user <- eval (\ctxt -> authenticateUser ctxt "new" "pass")
      userUsername user `shouldEqual` "new"
    it "should return Nothing if the email is wrong" $ do
      void $ eval (\ctxt -> createUser ctxt
                            (NewUser "new" "new@new.com" "pass"))
      eval (\ctxt -> authenticateUser ctxt "eew" "pass")
        >>= shouldEqual Nothing
    it "should return Nothing if the password is wrong" $ do
      void $ eval (\ctxt -> createUser ctxt
                            (NewUser "new" "new@new.com" "pass"))
      eval (\ctxt -> authenticateUser ctxt "new" "random")
        >>= shouldEqual Nothing
