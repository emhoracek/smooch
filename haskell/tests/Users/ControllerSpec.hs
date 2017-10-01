{-# LANGUAGE OverloadedStrings #-}

module Users.ControllerSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Fn

import           Common
import           Ctxt
import           Web

spec :: Spec
spec = fnTests $
  describe "/users" $ do
    it "should list all the users" $ do
      get "/users" >>= should200
