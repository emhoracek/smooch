{-# LANGUAGE OverloadedStrings #-}

module ShellSpec (spec) where

import Test.Hspec
import Shell
import Turtle
import System.Process

spec = do
  describe "convertCell" $
    it "converts a cel to png" $ 
      pendingWith "IO!! Oh no!!"
  describe "convertCels" $
    it "finds the transparent color and converts all the cels" $
      pendingWith "IO!! Oh no!!"
  describe "transColor" $ do
    it "gets the transparent color from a palette" $
      transColor "tests/samples/color.kcf" `shouldReturn` "rgb:ff/f7/ff"
      
