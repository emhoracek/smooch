{-# LANGUAGE OverloadedStrings #-}

module Users.ControllerSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Fn

import           Common
import           Users.Model

spec :: Spec
spec = fnTests $ do
  describe "/users" $ do
    it "should list all the users" $ do
      get "/users" >>= should200

  describe "/users/create" $ do
    it "should create a new user" $ do
      post "/users/create" [("username", "new")
                           ,("email", "new@test.com")
                           ,("password", "pass")
                           ,("password-confirmation", "pass")]
        >>= should200
      [user] <- eval (\ctxt -> getUsers ctxt)
      userEmail user `shouldEqual` "new@test.com"

    it "should not create a new user if password doesn't match" $ do
      post "/users/create" [("username", "new")
                           ,("email", "new@test.com")
                           ,("password", "pass")
                           ,("password-confirmation", "random")]
        >>= should200
      eval (\ctxt -> getUsers ctxt)
        >>= shouldEqual []

    it "should not create a new user if email is invalid" $ do
      post "/users/create" [("username", "new")
                           ,("email", "newtest.com")
                           ,("password", "pass")
                           ,("password-confirmation", "random")]
        >>= should200
      eval (\ctxt -> getUsers ctxt)
        >>= shouldEqual []
