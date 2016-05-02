{-# LANGUAGE OverloadedStrings #-}

module ShellSpec (spec) where

import           Control.Monad.Trans.Either
import           Data.Monoid                ((<>))
import           Shell
import           System.Process
import           Test.Hspec

sampleDir = "tests/samples"

spec = do
  describe "convertCel" $ do
    it "converts a cel to png" $
      runEitherT (convertCel "color.kcf" "aurora" "rgb:ff/f7/ff" (sampleDir <> "/aurora")) `shouldReturn` Right ("aurora", (0,0))
    it "gives an error if the file isn't there" $
      runEitherT (convertCel "color.kcf" "aurora.cel" "rgb:ff/f7/ff" (sampleDir <> "/aurora")) `shouldReturn`
        Left "Error while converting cel tests/samples/aurora/aurora.cel.cel. Exit code: 1. Error: Read palette tests/samples/aurora/color.kcf \nNew style palette\nRead cel tests/samples/aurora/aurora.cel.cel \ntests/samples/aurora/aurora.cel.cel: No such file or directory\n"

  describe "convertCels" $ do
    it "finds the transparent color and converts all the cels" $
      runEitherT (convertCels "color.kcf" ["aurora"] (sampleDir <> "/aurora")) `shouldReturn` Right [("aurora", (0,0))]
    it "returns an error if a cel is missing" $
      runEitherT (convertCels "color.kcf" ["aurora.cel"] (sampleDir <> "/aurora")) `shouldReturn`
        Left "Error while converting cel tests/samples/aurora/aurora.cel.cel. Exit code: 1. Error: Read palette tests/samples/aurora/color.kcf \nNew style palette\nRead cel tests/samples/aurora/aurora.cel.cel \ntests/samples/aurora/aurora.cel.cel: No such file or directory\n"


  describe "transColor" $ do
    it "gets the transparent color from a palette" $
      runEitherT (transColor (sampleDir <> "/aurora/color.kcf")) `shouldReturn` Right "rgb:ff/f7/ff"
    it "returns an error if the palette isn't there" $
      runEitherT (transColor "potato") `shouldReturn`
        Left "Error while finding transparency color. Exit code: 1. Error: Read palette potato \npotato: No such file or directory\n"
