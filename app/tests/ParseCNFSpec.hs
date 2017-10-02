module ParseCNFSpec (spec) where

import           Control.Monad.Trans.Either
import           Test.Hspec
import           Text.ParserCombinators.Parsec

import           Kiss
import           ParseCNF

fakeKissCell :: Int -> String -> Int -> [Int] -> Int -> KissCell
fakeKissCell a s b cs d = KissCell a s b cs d (Position 0 0)

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

sampleKiss :: String
sampleKiss =
  "; ** Palette files ** \n" ++
  "%colors.kcf \n" ++
  "(756,398) \n" ++
  "[0 \n" ++
  "#1   shirt.cel  *0 : 0 1 2 3 \n" ++
  "#2   body.cel   *0 : 0 1 2 3 \n" ++
  "#3.   shirtb.cel *0 : 0 1 2 3 \n" ++
  "$0 * 1,1 2,2 3,3"

sampleKiss2 :: String
sampleKiss2 =
  "; ** Palette files ** \n" ++
  "%colors.kcf \n" ++
  "(756,398) \n" ++
  "[0 \n" ++
  "#1   shirt.cel  *0 : 0 1 2 3 \n" ++
  "#1   shirtb.cel *0 : 0 1 2 3 \n" ++
  "$0 * 0,0"

sampleKiss3 :: String
sampleKiss3 =
  "; ** Palette files ** \n" ++
  "%colors.kcf \n" ++
  "(756,398) \n" ++
  "[0 \n" ++
  "#1   shirt.cel  *0 : 0 1 2 3 \n" ++
  "#2   body.Cel   *0 : 0 1 2 3 \n" ++
  "#1   shirtb.CEL *0 : 0 1 2 3 \n" ++
  "$0 * 1,1 2,2"

sampleKiss4 :: String
sampleKiss4 =
  "; ** Palette files ** \n" ++
  "%colors.kcf \n" ++
  "(756,398) \n" ++
  "[ ; lol no border number \n" ++
  "#1   shirt.cel  *0 : 0 1 2 3 \n" ++
  "#2   body.Cel   *0 : 0 1 2 3 \n" ++
  "; let's just throw some random numbers in the next line \n" ++
  "#1   shirtb.CEL  3234 \n" ++
  "$0 * 1,1 2,2 \n" ++
  "; better end this with file separator character \n" ++
  ['\FS']

spec :: Spec
spec = do
  describe "getKissData" $ do
    it "parses a CNF into KiSS data" $
      runEitherT (getKissData sampleKiss) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ KissObject 1 [ fakeKissCell 0 "shirt" 0 [0,1,2,3] 0] [Position 1 1],
            KissObject 2 [ fakeKissCell 0 "body"  0 [0,1,2,3] 0] [Position 2 2],
            KissObject 3 [ fakeKissCell 0 "shirtb" 0 [0,1,2,3] 0] [Position 3 3]])
    it "parses a CNF into KiSS data" $
      runEitherT (getKissData sampleKiss2) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ KissObject 1 [ fakeKissCell 0 "shirt" 0 [0,1,2,3] 0,
                           fakeKissCell 0 "shirtb" 0 [0,1,2,3] 0 ] [Position 0 0]])
    it "parses a CNF into KiSS data even with idiosyncratic caps" $
      runEitherT (getKissData sampleKiss3) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ KissObject 1 [ fakeKissCell 0 "shirt" 0 [0,1,2,3] 0,
                           fakeKissCell 0 "shirtb" 0 [0,1,2,3] 0 ] [Position 1 1],
            KissObject 2 [ fakeKissCell 0 "body"  0 [0,1,2,3] 0] [Position 2 2] ])
    it "parses a CNF into KiSS data even with odd formatting/encoding" $
      runEitherT (getKissData sampleKiss4) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ KissObject 1 [ fakeKissCell 0 "shirt" 0 [0,1,2,3] 0,
                           fakeKissCell 0 "shirtb" 0 [0..9] 0 ] [Position 1 1],
            KissObject 2 [ fakeKissCell 0 "body"  0 [0,1,2,3] 0] [Position 2 2] ])

    it "returns an error message for a bad cnf" $
      pendingWith "oops"
{--
      getKissData "I'm not a CNF." `shouldBe`
   --}
  describe "getKissCells" $
    it "parses a CNF into a list of KiSS cels" $
      runEitherT (getKissCels sampleKiss) `shouldReturn`
         Right [ CNFKissCell 0 "shirt" 0 [0,1,2,3] 0,
            CNFKissCell 0 "body"  0 [0,1,2,3] 0,
            CNFKissCell 0 "shirtb" 0 [0,1,2,3] 0 ]
  describe "parseCNFLine" $ do
    it "parses a single KiSS cel" $
      parse parseCNFLine "blah" sampleCell1 `shouldBe`
        Right (CNFCell (19, CNFKissCell 0 "markr6" 0 [0,1,2,3,4,5,6,7,8,9] 0))
    it "parses a single KiSS cel (with transparency)" $
      parse parseCellLine "blah" sampleCell2 `shouldBe`
        Right (CNFCell (25, CNFKissCell 0 "tights1" 0 [0,1,2,3,4,5,6,7,8,9] 75))
    it "parses a single KiSS cel (different sets and fix val)" $
      parse parseCellLine "blah" sampleCell3 `shouldBe`
        Right (CNFCell (180, CNFKissCell 99 "handl" 0 [1,4,7] 0))
    it "parses a the positions and palette for a single set in a set" $
      parse parseCNFLine "blah" sampleSet1 `shouldBe`
        Right (CNFSetPos (KissSetPos 2
                          [Position 192 11, NoPosition, Position 56 176, Position 55 21]))
    it "parses a set a with line breaks" $
      parse parseCNFLine "blah" sampleSet2 `shouldBe`
        Right (CNFSetPos (KissSetPos 3
                          [NoPosition, Position 165 207, NoPosition, Position 125 261]))
    it "parses a set with a line comment" $
      parse parseCNFLine "blah" sampleSet3 `shouldBe`
        Right (CNFSetPos (KissSetPos 0
                          [Position 192 11, NoPosition, Position 56 17]))

    it "joins cels into objects" $
      linesToObjects [(1, CNFKissCell 0 "shirt" 0 [0,1,2,3] 0),
                      (2, CNFKissCell 1 "body" 0 [0,1,2,3] 0),
                      (1, CNFKissCell 0 "shirtb" 0 [0,1,2,3] 0) ]
                     [KissSetPos 0 [NoPosition, Position 200 200, Position 100 100],
                      KissSetPos 0 [NoPosition, Position 100 100, Position 100 100 ]] `shouldBe`
      [ KissObject 1 [ fakeKissCell 0 "shirt" 0 [0,1,2,3] 0,
                       fakeKissCell 0 "shirtb" 0 [0,1,2,3] 0 ]
        [Position 200 200, Position 100 100],
        KissObject 2 [ fakeKissCell 1 "body" 0 [0,1,2,3] 0]
        [Position 100 100, Position 100 100] ]

    it "combines cels and adds positions to objs" $
      combineCelsAndPositions
        [ (1, CNFKissCell 0 "shirt" 0 [0,1,2,3] 0),
          (1, CNFKissCell 0 "shirtb" 0 [0,1,2,3] 0),
          (2, CNFKissCell 1 "body" 0 [0,1,2,3] 0)]
        [(0, [NoPosition]),
         (1, [Position 200 200]),
         (2, [Position 100 100])] `shouldBe`
        [(1, [CNFKissCell 0 "shirt" 0 [0,1,2,3] 0,
              CNFKissCell 0 "shirtb" 0 [0,1,2,3] 0], [Position 200 200]),
         (2, [ CNFKissCell 1 "body" 0 [0,1,2,3] 0], [Position 100 100])]
