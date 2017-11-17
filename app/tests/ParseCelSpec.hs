{-|
Module      : ParseCelSpec
Description : Hspec tests for ParseCel
Copyright   : (c) Huggable Monad, 2017
License     : GPL-3
Maintainer  : huggablemonad@hushmail.me
Stability   : experimental
Portability : POSIX

This module provides Hspec tests for the 'ParseCel' module.
-}

{-# LANGUAGE OverloadedStrings #-}

module ParseCelSpec (spec) where

import qualified Control.Monad.Trans.Either as ET
import           Data.Bits                  ((.&.))
import qualified Data.Bits                  as Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Word                  (Word16, Word8)
import           Test.Hspec                 (Spec, describe, it, shouldReturn)

import           ParseCel

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

-- | Return a new-style cel header.
makeHeader :: Word8 -- ^ Filemark.
           -> Word8 -- ^ Bits-per-pixel.
           -> Word16 -- ^ Width (1-XMAX).
           -> Word16 -- ^ Height (1-YMAX).
           -> Word16 -- ^ x-offset (0-(XMAX-1)).
           -> Word16 -- ^ y-offset (0-(YMAX-1)).
           -> ByteString
makeHeader filemark bpp width height xOffset yOffset =
  let kissId = [0x4B, 0x69, 0x53, 0x53]
      filemark' = [filemark]
      unused1 = [0x00, 0x00]
      width' = splitWord16 width
      height' = splitWord16 height
      xOffset' = splitWord16 xOffset
      yOffset' = splitWord16 yOffset
      unused2 = replicate 16 0x00
      header = [ kissId, filemark', [bpp], unused1, width', height'
               , xOffset', yOffset', unused2]
  in BS.pack . concat $ header

-- | Return an old-style cel header.
makeOldHeader :: Word16 -- ^ Width (1-XMAX).
              -> Word16 -- ^ Height (1-YMAX).
              -> ByteString
makeOldHeader width height =
  let width' = splitWord16 width
      height' = splitWord16 height
  in BS.pack . concat $ [width', height']

-- * Valid cels

-- | New style, 8 bpp, width 1, height 1, x-offset 0, and y-offset 0.
validNewCel1 :: ByteString
validNewCel1 =
  let header = makeHeader 0x20 0x08 0x01 0x01 0x00 0x00
      celData = BS.singleton 0x00
  in BS.concat [header, celData]

-- | New style, 4 bpp, width 1, height 1, x-offset 0, and y-offset 0.
validNewCel2 :: ByteString
validNewCel2 =
  let header = makeHeader 0x20 0x04 0x01 0x01 0x00 0x00
      celData = BS.singleton 0x00
  in BS.concat [header, celData]

-- | New style, 8 bpp, width 2, height 1, x-offset 0, and y-offset 0.
validNewCel3 :: ByteString
validNewCel3 =
  let header = makeHeader 0x20 0x08 0x02 0x01 0x00 0x00
      celData = BS.pack [0x00, 0x01]
  in BS.concat [header, celData]

-- | Old style, 4 bpp, width 1, height 1, x-offset 0, and y-offset 0.
validOldCel1 :: ByteString
validOldCel1 =
  let header = makeOldHeader 1 1
      celData = BS.pack [0x00]
  in BS.concat [header, celData]

-- * Invalid cels

-- | New style with invalid filemark.
invalidNewCel1 :: ByteString
invalidNewCel1 =
  let header = makeHeader 0xFF 0x08 0x01 0x01 0x00 0x00
      celData = BS.pack [0x00]
  in BS.concat [header, celData]

-- | New style with invalid bits-per-pixel.
invalidNewCel2 :: ByteString
invalidNewCel2 =
  let header = makeHeader 0x20 0xFF 0x01 0x01 0x00 0x00
      celData = BS.pack [0x00]
  in BS.concat [header, celData]

-- | New style with insufficient cel pixels.
invalidNewCel3 :: ByteString
invalidNewCel3 =
  let header = makeHeader 0x20 0x08 0x02 0x01 0x00 0x00
      celData = BS.pack [0x00]
  in BS.concat [header, celData]

-- | New style with extra cel pixels.
invalidNewCel4 :: ByteString
invalidNewCel4 =
  let header = makeHeader 0x20 0x08 0x01 0x01 0x00 0x00
      celData = BS.pack [0x00, 0xFF]
  in BS.concat [header, celData]

-- | New style with invalid width.
invalidNewCel5 :: ByteString
invalidNewCel5 =
  let header = makeHeader 0x20 0x08 0x00 0x01 0x00 0x00
      celData = BS.singleton 0x00
  in BS.concat [header, celData]

-- | New style with invalid height.
invalidNewCel6 :: ByteString
invalidNewCel6 =
  let header = makeHeader 0x20 0x08 0x01 0x00 0x00 0x00
      celData = BS.singleton 0x00
  in BS.concat [header, celData]

-- | Old style with insufficient cel pixels.
invalidOldCel1 :: ByteString
invalidOldCel1 = makeOldHeader 1 1

-- | Old style with extra cel pixels.
invalidOldCel2 :: ByteString
invalidOldCel2 =
  let header = makeOldHeader 1 1
      celData = BS.pack [0x00, 0xFF]
  in BS.concat [header, celData]

-- | Old style with invalid width.
invalidOldCel3 :: ByteString
invalidOldCel3 =
  let header = makeOldHeader 0 1
      celData = BS.singleton 0x00
  in BS.concat [header, celData]

-- | Old style with invalid height.
invalidOldCel4 :: ByteString
invalidOldCel4 =
  let header = makeOldHeader 1 0
      celData = BS.singleton 0x00
  in BS.concat [header, celData]

-- * Tests

-- | Test suite for running all the tests.
spec :: Spec
spec = testParseCel

-- | Test 'parseCel'.
testParseCel :: Spec
testParseCel =
  describe "parseCel" $ do
    it "parses new cel (8/1/1/0/0) into CelPixels" $
      checkLength validNewCel1 `shouldReturn`
        Right 1
    it "parses new cel (4/1/1/0/0) into CelPixels" $
      checkLength validNewCel2 `shouldReturn`
        Right 1
    it "parses new cel (8/2/1/0/0) into CelPixels" $
      checkLength validNewCel3 `shouldReturn`
        Right 2
    it "parses old cel (4/1/1/0/0) into CelPixels" $
      checkLength validOldCel1 `shouldReturn`
        Right 1
    it "returns an error message for empty cel" $
      runParseCel BS.empty `shouldReturn`
        Left "End of input"
    it "returns an error message for new cel with invalid filemark" $
      runParseCel invalidNewCel1 `shouldReturn`
        Left "Filemark should be 0x20 and is actually 0xff"
    it "returns an error message for new cel with invalid bpp" $
      runParseCel invalidNewCel2 `shouldReturn`
        Left "Invalid bits-per-pixel of 255"
    it "returns an error message for new cel with truncated cel data" $
      runParseCel invalidNewCel3 `shouldReturn`
        Left "End of input"
    it "returns an error message for new cel with extra cel data" $
      runParseCel invalidNewCel4 `shouldReturn`
        Left "Not the end of input"
    it "returns an error message for new cel with invalid width" $
      runParseCel invalidNewCel5 `shouldReturn`
        Left "Invalid width or height"
    it "returns an error message for new cel with invalid height" $
      runParseCel invalidNewCel6 `shouldReturn`
        Left "Invalid width or height"
    it "returns an error message for old cel with truncated cel data" $
      runParseCel invalidOldCel1 `shouldReturn`
        Left "End of input"
    it "returns an error message for old cel with extra cel data" $
      runParseCel invalidOldCel2 `shouldReturn`
        Left "Not the end of input"
    it "returns an error message for old cel with invalid width" $
      runParseCel invalidOldCel3 `shouldReturn`
        Left "Invalid width or height"
    it "returns an error message for old cel with invalid height" $
      runParseCel invalidOldCel4 `shouldReturn`
        Left "Invalid width or height"
  where runParseCel cel = ET.runEitherT (parseCel cel)
        checkLength cel = fmap (lengthCelPixels . snd) <$> runParseCel cel
