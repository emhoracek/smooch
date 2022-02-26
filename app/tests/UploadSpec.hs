{-# LANGUAGE OverloadedStrings #-}

module UploadSpec where

import           Control.Monad.Trans.Except
import           Test.Hspec

import           Upload

spec :: Spec
spec = do
  describe "getCNF" $ do
    it "returns the contents of the first CNF file it finds" $
      runExceptT (getCNF "tests/samples") `shouldReturn` Right "okay\n"
    it "gives an error if no CNF is found" $
      runExceptT (getCNF "./") `shouldReturn` Left "No configuration file found."
