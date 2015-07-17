module ParseCNFSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec
import ParseCNF
import Kiss

sampleCell1 :: String
sampleCell1 = "#19       markr6.cel      *0 : 0 1 2 3 4 5 6 7 8 9"
sampleCell2 :: String
sampleCell2 = "#25       tights1.cel     *0 : 0 1 2 3 4 5 6 7 8 9     ;%t75 &"
sampleCell3 :: String
sampleCell3 = "#180.99   handl.cel       *0 :   1     4     7 &"

sampleSet1 :: String 
sampleSet1 = "$2 192,11 * 56,176 55,21"
sampleSet2 :: String
sampleSet2 = "$3 * 165,207 \n * 125,261"
sampleSet3 :: String
sampleSet3 = "$0 192,11 * 56,17 ; blah"

spec = do
  describe "getKissData" $
    it "parses a CNF into KiSS data" $
      pendingWith "laziness"
  describe "getKissCells" $
    it "parses a CNF into a list of KiSS cels" $
      pendingWith "laziness"
  describe "parseCNFLine" $ do
    it "parses a single KiSS cel" $ 
      parse parseCNFLine "blah" sampleCell1 `shouldBe`
        Right (CNFCell (19, KissCell 0 "markr6" 0 [0,1,2,3,4,5,6,7,8,9] 0))
    it "parses a single KiSS cel (with transparency)" $
      parse parseCellLine "blah" sampleCell2 `shouldBe`
        Right (CNFCell (25, KissCell 0 "tights1" 0 [0,1,2,3,4,5,6,7,8,9] 75))
    it "parses a single KiSS cel (different sets and fix val)" $
      parse parseCellLine "blah" sampleCell3 `shouldBe`
        Right (CNFCell (180, KissCell 99 "handl" 0 [1,4,7] 0))
    it "parses a the positions and palette for a single set in a set" $
      parse parseCNFLine "blah" sampleSet1 `shouldBe`
        Right (CNFSetPos (KissSetPos 2 
                          [Position 192 11, NoPosition, Position 56 176, Position 55 21]))
{--
sampleSet1 :: String 
sampleSet1 = "$2 192,11 * 56,176 55,21"
sampleSet2 :: String
sampleSet2 = "$3 * 165,207 \n * 125,261"
sampleSet3 :: String
sampleSet3 = "$0 192,11 * 56,17 ; blah"
--}
