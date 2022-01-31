{-# LANGUAGE OverloadedStrings #-}

module WebSpec (spec) where

import           Control.Monad (void)
import           Test.Hspec
import           Test.Hspec.Fn

import           Common
import           Users.Model

spec :: Spec
spec = fnTests $ do
  describe "/login" $ do
    it "should login the user" $ do
      let newUser = NewUser "new" "new@new.com" "pass"
      void $ eval (\ctxt -> createUser ctxt newUser)
      post "/login" [("username", "new")
                    ,("password", "pass")]
        >>= should300To "/users/new"
    it "shouldn't login the user if the password is wrong" $ do
      let newUser = NewUser "new" "new@new.com" "pass"
      void $ eval (\ctxt -> createUser ctxt newUser)
      post "/login" [("username", "new")
                    ,("password", "passbad")]
        >>= shouldNot300
    it "shouldn't login the user if the username is wrong" $ do
      let newUser = NewUser "new" "new@new.com" "pass"
      void $ eval (\ctxt -> createUser ctxt newUser)
      post "/login" [("username", "newblah")
                    ,("password", "pass")]
        >>= shouldNot300
