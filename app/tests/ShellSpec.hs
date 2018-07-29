{-# LANGUAGE OverloadedStrings #-}

module ShellSpec (spec) where

import           Control.Monad.Trans.Either
import qualified Data.Array                 as A
import           Data.Monoid                ((<>))
import           Test.Hspec

import           Shell
import           Kiss

sampleDir :: [Char]
sampleDir = "tests/samples"

samplePalettes :: Palettes
samplePalettes = toArray ["color.kcf"]
  where toArray l = A.listArray (0, length l) l

fakeCel :: CNFKissCel
fakeCel = CNFKissCel 0 "aurora" 0 [] 0

spec :: Spec
spec = do
  describe "convertCel" $ do
    it "converts a cel to png" $
      runEitherT (convertCel samplePalettes 0 "aurora" (sampleDir <> "/aurora")) `shouldReturn` Right ("aurora", (0,0))
    it "gives an error if the file isn't there" $
      runEitherT (convertCel samplePalettes 0 "aurora.cel" (sampleDir <> "/aurora")) `shouldReturn`
        Left "Error while converting cel tests/samples/aurora/aurora.cel.cel. Exit code: 1. Error: Read palette tests/samples/aurora/color.kcf \nNew style palette\nRead cel tests/samples/aurora/aurora.cel.cel \ntests/samples/aurora/aurora.cel.cel: No such file or directory\n"

  describe "convertCels" $ do
    it "finds the transparent color and converts all the cels" $
      runEitherT (convertCels samplePalettes [fakeCel] (sampleDir <> "/aurora")) `shouldReturn` Right [("aurora", (0,0))]
    it "returns an error if a cel is missing" $
      runEitherT (convertCels samplePalettes [fakeCel { cnfCelName = "aurora.cel"}] (sampleDir <> "/aurora")) `shouldReturn`
        Left "Error while converting cel tests/samples/aurora/aurora.cel.cel. Exit code: 1. Error: Read palette tests/samples/aurora/color.kcf \nNew style palette\nRead cel tests/samples/aurora/aurora.cel.cel \ntests/samples/aurora/aurora.cel.cel: No such file or directory\n"


  describe "transColor" $ do
    it "gets the transparent color from a palette" $
      runEitherT (transColor (sampleDir <> "/aurora/color.kcf")) `shouldReturn` Right "rgb:ff/f7/ff"
    it "returns an error if the palette isn't there" $
      runEitherT (transColor "potato") `shouldReturn`
        Left "Error while finding transparency color. Exit code: 1. Error: Read palette potato \npotato: No such file or directory\n"
  describe "borderColor" $ do
    it "gets the transparent color from a palette" $
      runEitherT (colorByIndex 3 (sampleDir <> "/aurora/color.kcf")) `shouldReturn` Right "#ffff94"
    it "returns an error if the palette isn't there" $
      runEitherT (colorByIndex 15 "potato") `shouldReturn`
        Left "Error while finding background color. Exit code: 1. Error: Read palette potato \npotato: No such file or directory\n"
