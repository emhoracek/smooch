module CreateHTMLSpec (spec) where

import Test.Hspec
import CreateHTML (createHTML)
import Kiss

sampleCels = [KissCell 0 "test" 0 [] 0] 

spec =
  describe "createHTML" $
    it "generates HTML from a list of KiSS cels" $
      createHTML sampleCels `shouldBe` 
        "<img src=\"test.png\" id=\"test\"> \n"
