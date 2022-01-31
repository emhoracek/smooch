{-|
Module      : ParseKCFSpec
Description : Hspec tests for ParseKCF
Copyright   : (c) Huggable Monad, 2017
License     : GPL-3
Maintainer  : huggablemonad@hushmail.me
Stability   : experimental
Portability : POSIX

This module provides Hspec tests for the 'ParseKCF' module.
-}

{-# LANGUAGE OverloadedStrings #-}

module ParseKCFSpec (spec) where

import qualified Control.Monad.Trans.Except as ET
import           Data.Bits                  ((.&.))
import qualified Data.Bits                  as Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Word                  (Word16, Word8)
import           Test.Hspec                 (Spec, describe, it, shouldReturn)

import           ParseKCF

-- * Utilities

-- | Split a 'Word16' into its constituent bytes in little endian order.
--
-- >>> -- [0xCD,0xAB] == [205,171].
-- >>> splitWord16 0xABCD
-- [205,171]
splitWord16 :: Word16 -> [Word8]
splitWord16 word =
  let msb = Bits.shiftR (word .&. 0xFF00) 8
      lsb = word .&. 0xFF
  in [fromIntegral lsb, fromIntegral msb]

-- | Return a new-style palette header.
makeHeader :: Word8 -- ^ Filemark.
           -> Word8 -- ^ Bits-per-pixel.
           -> Word16 -- ^ No. of colors in one palette group (1-256).
           -> Word16 -- ^ No. of palette groups (1-10).
           -> ByteString
makeHeader filemark bpp colors groups =
  let kissId = [0x4B, 0x69, 0x53, 0x53]
      filemark' = [filemark]
      unused1 = [0x00, 0x00]
      colors' = splitWord16 colors
      groups' = splitWord16 groups
      unused2 = replicate 20 0x00
      header = [kissId, filemark', [bpp], unused1, colors', groups', unused2]
  in BS.pack . concat $ header

-- * Valid palettes

-- | New style, 24 bpp, 1 color, and 1 palette group.
validNewPal1 :: ByteString
validNewPal1 =
  let header = makeHeader 0x10 0x18 0x01 0x01
      palData = BS.pack [0xF0, 0xF8, 0xFF]
  in BS.concat [header, palData]

-- | New style, 12 bpp, 1 color, and 1 palette group.
validNewPal2 :: ByteString
validNewPal2 =
  let header = makeHeader 0x10 0x0C 0x01 0x01
      palData = BS.pack [0xF0, 0xF8]
  in BS.concat [header, palData]

-- | New style, 24 bpp, 2 colors, and 1 palette group.
validNewPal3 :: ByteString
validNewPal3 =
  let header = makeHeader 0x10 0x18 0x02 0x01
      palData = BS.pack [0xF0, 0xF8, 0xFF, 0x9A, 0xCD, 0x32]
  in BS.concat [header, palData]

-- | New style with extra palette entries.
validNewPal4 :: ByteString
validNewPal4 =
  let header = makeHeader 0x10 0x18 0x01 0x01
      palData = BS.pack [0xF0, 0xF8, 0xFF, 0xA9, 0xA9, 0xA9]
  in BS.concat [header, palData]

-- | Old style, 12 bpp, 16 colors, and 10 palette groups.
validOldPal1 :: ByteString
validOldPal1 = BS.pack $ replicate 320 0xAB

-- | Old style with extra palette entries.
validOldPal2 :: ByteString
validOldPal2 = BS.pack $ replicate 321 0xF0

-- * Invalid palettes

-- | New style with invalid filemark.
invalidNewPal1 :: ByteString
invalidNewPal1 =
  let header = makeHeader 0xFF 0x18 0x01 0x01
      palData = BS.pack [0xF0, 0xF8, 0xFF]
  in BS.concat [header, palData]

-- | New style with invalid bits-per-pixel.
invalidNewPal2 :: ByteString
invalidNewPal2 =
  let header = makeHeader 0x10 0xFF 0x01 0x01
      palData = BS.pack [0xF0, 0xF8, 0xFF]
  in BS.concat [header, palData]

-- | New style with insufficient palette entries.
invalidNewPal3 :: ByteString
invalidNewPal3 =
  let header = makeHeader 0x10 0x18 0x01 0x01
      palData = BS.pack [0xF0, 0xF8]
  in BS.concat [header, palData]

-- | Old style with insufficient palette entries.
invalidOldPal1 :: ByteString
invalidOldPal1 = BS.pack $ replicate 31 0xF0

-- * Tests

-- | Test suite for running all the tests.
spec :: Spec
spec = do
  testParseKCF
  testColorByIndex
  testPalEntryByIndex

-- | Test 'parseKCF'.
testParseKCF :: Spec
testParseKCF =
  describe "parseKCF" $ do
    it "parses new KCF (24/1/1) into [PalEntry]" $
      checkLength validNewPal1 `shouldReturn`
        Right 1
    it "parses new KCF (12/1/1) into [PalEntry]" $
      checkLength validNewPal2 `shouldReturn`
        Right 1
    it "parses new KCF (24/2/1) into [PalEntry]" $
      checkLength validNewPal3 `shouldReturn`
        Right 2
    it "parses new KCF with extra palette data" $
      checkLength validNewPal4 `shouldReturn`
        Right 1
    it "parses old KCF into [PalEntry]" $
      checkLength validOldPal1 `shouldReturn`
        Right 16
    it "parses old KCF with extra palette data" $
      checkLength validOldPal2 `shouldReturn`
        Right 16
    it "returns an error message for empty KCF" $
      runParseKCF BS.empty `shouldReturn`
        Left "End of input"
    it "returns an error message for new KCF with invalid filemark" $
      runParseKCF invalidNewPal1 `shouldReturn`
        Left "Filemark should be 0x10 and is actually 0xff"
    it "returns an error message for new KCF with invalid bpp" $
      runParseKCF invalidNewPal2 `shouldReturn`
        Left "Invalid bits-per-pixel of 255"
    it "returns an error message for new KCF with truncated palette data" $
      runParseKCF invalidNewPal3 `shouldReturn`
        Left "End of input"
    it "returns an error message for old KCF with truncated palette data" $
      runParseKCF invalidOldPal1 `shouldReturn`
        Left "End of input"
  where runParseKCF pal = ET.runExceptT (parseKCF pal)
        checkLength pal = fmap lengthPalEntries <$> runParseKCF pal

-- | Test 'colorByIndex'.
testColorByIndex :: Spec
testColorByIndex =
  describe "colorByIndex" $ do
    it "returns the indexed color for new KCF (24/2/2)" $
      runColorByIndex 1 validNewPal3 `shouldReturn`
        Right "#9acd32"
    it "returns #000000 if the index is less than 0" $
      runColorByIndex (-1) validNewPal3 `shouldReturn`
        Right "#000000"
    it "returns #000000 if the index is more than the number of palette entries" $
      runColorByIndex 10 validNewPal3 `shouldReturn`
        Right "#000000"
  where runColorByIndex index pal =
          fmap (colorByIndex index) <$> ET.runExceptT (parseKCF pal)

-- | Test 'palEntryByIndex'.
testPalEntryByIndex :: Spec
testPalEntryByIndex =
  describe "palEntryByIndex" $ do
    it "returns the indexed PalEntry for new KCF (24/2/2)" $
      runPalEntryByIndex 1 validNewPal3 `shouldReturn`
        Right PalEntry {palRed = 154, palGreen = 205, palBlue = 50}
    it "returns PalEntry 0 0 0 if the index is less than 0" $
      runPalEntryByIndex (-1) validNewPal3 `shouldReturn`
        Right PalEntry {palRed = 0, palGreen = 0, palBlue = 0}
    it "returns PalEntry 0 0 0 if the index is more than the number of palette entries" $
      runPalEntryByIndex 10 validNewPal3 `shouldReturn`
        Right PalEntry {palRed = 0, palGreen = 0, palBlue = 0}
  where runPalEntryByIndex index pal =
          fmap (palEntryByIndex index) <$> ET.runExceptT (parseKCF pal)
