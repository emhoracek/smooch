{-# LANGUAGE OverloadedStrings #-}

module ShellSpec (spec) where

import Test.Hspec
import Shell
import System.Process
import Data.Monoid ((<>))

sampleDir = "tests/samples"

spec = do
  describe "convertCel" $ do
    it "converts a cel to png" $ 
      convertCel "color.kcf" "aurora" "rgb:ff/f7/ff" (sampleDir <> "/aurora") `shouldReturn` Right ()
    it "gives an error if the file isn't there" $
      convertCel "color.kcf" "aurora.cel" "rgb:ff/f7/ff" (sampleDir <> "/aurora") `shouldReturn`
        Left "Error while analyzing cel tests/samples/aurora/aurora.cel.cel. Exit code: 139. Error: Read palette tests/samples/aurora/color.kcf \nNew style palette\nRead cel tests/samples/aurora/aurora.cel.cel \nSegmentation fault (core dumped)\n"
 
  describe "convertCels" $ do
    it "finds the transparent color and converts all the cels" $
      convertCels "color.kcf" ["aurora"] (sampleDir <> "/aurora") `shouldReturn` Right ()
    it "returns an error if a cel is missing" $ 
      convertCels "color.kcf" ["aurora.cel"] (sampleDir <> "/aurora") `shouldReturn` 
        Left "Error while analyzing cel tests/samples/aurora/aurora.cel.cel. Exit code: 139. Error: Read palette tests/samples/aurora/color.kcf \nNew style palette\nRead cel tests/samples/aurora/aurora.cel.cel \nSegmentation fault (core dumped)\n"
 
  describe "transColor" $ do
    it "gets the transparent color from a palette" $
      transColor (sampleDir <> "/aurora/color.kcf") `shouldReturn` Right "rgb:ff/f7/ff"
    it "returns an error if the palette isn't there" $
      transColor "potato" `shouldReturn` Left "Error while finding transparency color. Exit code: 139. Error: Read palette potato \nSegmentation fault (core dumped)\n"       
