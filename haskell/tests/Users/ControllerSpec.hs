{-# LANGUAGE OverloadedStrings #-}

module Users.ControllerSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Fn

import           Ctxt
import           Web

spec :: Spec
spec = do
  ctxt <- runIO initializer
  fn (return ctxt) appBase [] (const $ return ()) spec'

spec' :: SpecWith (FnHspecState Ctxt)
spec' =
  describe "/users" $ do
    it "should list all the users" $ do
      get "/users" >>= should200
