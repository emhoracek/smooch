module CreateHTMLSpec (spec) where

import Test.Hspec
import CreateHTML

spec =
  describe "htmlCelsImages" $
    it "generates HTML from a list of KiSS cels" $
      pendingWith "laziness"
