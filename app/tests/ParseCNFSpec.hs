{-# LANGUAGE OverloadedStrings #-}
module ParseCNFSpec (spec) where

import           Control.Monad.Trans.Except
import           Test.Hspec
import           Text.ParserCombinators.Parsec

import           Kiss
import           ParseCNF

fakeKissCel :: Int -> Int -> String -> Int -> [Int] -> Int -> KissCel
fakeKissCel e a s b cs d = KissCel e a s b cs d (Position 0 0)

sampleCel1 :: String
sampleCel1 = "#19       markr6.cel      *0 : 0 1 2 3 4 5 6 7 8 9 \n"
sampleCel2 :: String
sampleCel2 = "#25       tights1.cel     *0 : 0 1 2 3 4 5 6 7 8 9     ;%t75 & \n"
sampleCel3 :: String
sampleCel3 = "#180.99   handl.cel       *0 :   1     4     7 & \n"

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
  "$0 * 1,1 2,2 3,3 "

sampleKiss2 :: String
sampleKiss2 =
  "; ** Palette files ** \n" ++
  "%colors.kcf \n" ++
  "(756,398) \n" ++
  "[0 \n" ++
  "#1   shirt.cel  *0 : 0 1 2 3 \n" ++
  "#1   shirtb.cel *0 : 0 1 2 3 \n" ++
  "$0 * 0,0 "

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
  "#1   SHIRTB.CEL  3234 \n" ++
  "$0 * 1,1 2,2 \n" ++
  "; what if I throw a sub in here!\n" ++
  ['\SUB', '\n'] ++
  "; better end this with file separator character \n" ++
  ['\FS']

sampleKiss5 :: String
sampleKiss5 = "\
  \#31 collar2.cel :    2 3 4 5 6 7 8 9 ;���r�o�@�����s�[�X��\n\
  \#33 fuku4b.cel :    2 3 4 5 6 7 8 9 ;���r�o�@�㒅\n\
  \#6 fuku1c.cel :0 1 2   4 5 6 7 8 9 :���P�@�@�H�D\n\
  \#32 belt4.cel :    2 3 4 5 6 7 8 9 ;���r�o�@��\n\
  \$0 * 110,31 \n\
  \ * * \n\
  \$0 * 110,31 \n\
  \ 275,147 223,109"

sampleKiss5Cels :: CNFKissData
sampleKiss5Cels =
  CNFKissData 0 0 [] (600, 480)
    [ KissCel 31 0 "collar2" 0 [2,3,4,5,6,7,8,9] 0 (Position 0 0)
    , KissCel 33 0 "fuku4b" 0 [2,3,4,5,6,7,8,9] 0 (Position 0 0)
    , KissCel 6 0 "fuku1c" 0 [0,1,2,4,5,6,7,8,9] 0 (Position 0 0)
    , KissCel 32 0 "belt4" 0 [2,3,4,5,6,7,8,9] 0 (Position 0 0) ]
    [ KissSetPos 0 [NoPosition, Position 110 31, NoPosition, NoPosition]
    , KissSetPos 0 [NoPosition, Position 110 31, Position 275 147, Position 223 109] ]
    []

sampleKiss6 :: String
sampleKiss6 = "\
  \%zero.kcf     - Mirror and Titles\n\
  \%male.kcf     - Male Body\n\
  \%female.kcf   - Female Body\n"

sampleKiss6Output :: [CNFLine]
sampleKiss6Output = [
  CNFPalette "zero.kcf",
  CNFPalette "male.kcf",
  CNFPalette "female.kcf"
  ]

sampleFKiss :: String
sampleFKiss = "\
\;@begin() \n\
\;@  timer(1, 3000)\n\
\;@alarm(1)\n\
\;@ map(\"blink.cel\")\n\
\;@ timer(2, 150)\n\
\;@alarm(2)\n\
\;@ unmap(\"blink.cel\")\n\
\;@ timer(3, 6500)\n\
\;@alarm(3)\n\
\;@ map(\"blink.cel\")\n\
\;@ timer(2, 150)\n"

fKiSSLines =
  [ CNFFKiSSEvent $ FKiSSEvent "begin" [] [
       FKiSSAction "timer" [Number 1, Number 3000]
    ]
  , CNFFKiSSEvent $ FKiSSEvent "alarm" [Number 1] [
       FKiSSAction "map" [Text "blink.cel"]
     , FKiSSAction "timer" [Number 2, Number 150]
    ]
  , CNFFKiSSEvent $ FKiSSEvent "alarm" [Number 2] [
       FKiSSAction "unmap" [Text "blink.cel"]
     , FKiSSAction "timer" [Number 3, Number 6500]
    ]
  , CNFFKiSSEvent $ FKiSSEvent "alarm" [Number 3] [
       FKiSSAction "map" [Text "blink.cel"]
     , FKiSSAction "timer" [Number 2, Number 150]
    ]
  ]

spec :: Spec
spec = do
  describe "getKissData" $ do
    it "parses a CNF into KiSS data" $
      runExceptT (getKissData sampleKiss) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ fakeKissCel 1 0 "shirt" 0 [0,1,2,3] 0,
            fakeKissCel 2 0 "body"  0 [0,1,2,3] 0,
            fakeKissCel 3 0 "shirtb" 0 [0,1,2,3] 0] [KissSetPos 0  [NoPosition, Position 1 1, Position 2 2, Position 3 3]]
          [])
    it "parses a CNF into KiSS data" $
      runExceptT (getKissData sampleKiss2) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ fakeKissCel 1 0 "shirt" 0 [0,1,2,3] 0,
            fakeKissCel 1 0 "shirtb" 0 [0,1,2,3] 0 ] [KissSetPos 0 [NoPosition, Position 0 0]]
            [])
    it "parses a CNF into KiSS data even with idiosyncratic caps" $
      runExceptT (getKissData sampleKiss3) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ fakeKissCel 1 0 "shirt" 0 [0,1,2,3] 0,
            fakeKissCel 2 0 "body"  0 [0,1,2,3] 0,
            fakeKissCel 1 0 "shirtb" 0 [0,1,2,3] 0 ]
            [KissSetPos 0 [NoPosition, Position 1 1, Position 2 2]]
            [])
    it "parses a CNF into KiSS data even with odd formatting/encoding" $
      runExceptT (getKissData sampleKiss4) `shouldReturn`
        Right (CNFKissData 0 0 ["colors.kcf"] (756, 398)
          [ fakeKissCel 1 0 "shirt" 0 [0,1,2,3] 0,
            fakeKissCel 2 0 "body"  0 [0,1,2,3] 0,
            fakeKissCel 1 0 "shirtb" 0 [0..9] 0] [KissSetPos 0 [NoPosition, Position 1 1, Position 2 2]]
          [])
    it "parces even worse broken syntax" $
      runExceptT (getKissData sampleKiss5) `shouldReturn`
         Right sampleKiss5Cels
    it "returns an error message for a bad cnf" $
      runExceptT (getKissData "I'm not a CNF.") `shouldReturn`
        Left "\"KiSS CNF error: \" (line 1, column 1):\nunexpected \"i\"\nexpecting \
             \\"#\", \"$\", \";\", \"%\", \"[\", \"(\", \";@EventHandler\", \";@\", \
             \\"=\" or whitespace"
  describe "getKissCels" $ do
    it "parses a CNF into a list of KiSS cels" $
      runExceptT (getKissCels sampleKiss) `shouldReturn`
         Right [ CNFKissCel 1 0 "shirt" 0 [0,1,2,3] 0,
                 CNFKissCel 2 0 "body"  0 [0,1,2,3] 0,
                 CNFKissCel 3 0 "shirtb" 0 [0,1,2,3] 0 ]
  describe "parseCNFLine" $ do
    it "parses a single KiSS cel" $
      parse parseCNFLine "blah" sampleCel1 `shouldBe`
        Right (CNFCel (CNFKissCel 19 0 "markr6" 0 [0,1,2,3,4,5,6,7,8,9] 0))
    it "parses a single KiSS cel (with transparency)" $
      parse parseCelLine "blah" sampleCel2 `shouldBe`
        Right (CNFCel (CNFKissCel 25 0 "tights1" 0 [0,1,2,3,4,5,6,7,8,9] 75))
    it "parses a single KiSS cel (different sets and fix val)" $
      parse parseCelLine "blah" sampleCel3 `shouldBe`
        Right (CNFCel (CNFKissCel 180 99 "handl" 0 [1,4,7] 0))
    it "parses the palettes even with junk at end of line" $
      parse parseCNFLines "blah" sampleKiss6 `shouldBe`
        Right sampleKiss6Output
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
  describe "parseCNFLines" $ do
    it "parses FKiSS events" $
      parse parseCNFLines "KiSS CNF error: " sampleFKiss `shouldBe`
        Right fKiSSLines
