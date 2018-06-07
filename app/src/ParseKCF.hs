{-|
Module      : ParseKCF
Description : Parse old- and new-style KiSS palettes
Copyright   : (c) Libby Horacek, 2015-2017
                  Huggable Monad, 2017
License     : GPL-3
Maintainer  : huggablemonad@hushmail.me
Stability   : experimental
Portability : POSIX

This module provides functions for parsing old- and new-style KiSS palettes.

It was ported from the @cel2pnm@ @C@ program written by Libby Horacek.
-}

{-# LANGUAGE OverloadedStrings #-}

module ParseKCF
    (
      -- * API
      PalEntries
    , parseKCF
    , colorByIndex
    , palEntryByIndex
    , lengthPalEntries

      -- * Palette data
    , PalEntry(..)
    ) where

import           BinaryParser               (BinaryParser)
import qualified BinaryParser               as BP
import qualified Control.Applicative        as CA
import           Control.Monad              as CM
import           Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as ET
import           Data.Bits                  ((.&.))
import qualified Data.Bits                  as Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import           Data.Word                  (Word16, Word8)
import           Formatting                 ((%), (%.))
import qualified Formatting                 as Fmt
import qualified Safe                       as Safe

-- * API

-- | Palette entries.
newtype PalEntries = PalEntries [PalEntry] deriving (Eq, Show)

-- | Parse a KiSS palette.
--
-- >>> runEitherT (parseKCF palette)
-- Right [PalEntry {palRed = 0, palGreen = 255, palBlue = 255}]
parseKCF :: ByteString -> EitherT Text IO PalEntries
parseKCF palData = do
    let headerStyle = if isNewStylePalette (BS.take 4 palData) then New else Old
    ET.hoistEither $ BP.run (parsePalette headerStyle) palData

-- | Return 'True' if the palette is new-style (starts with @KiSS@).
isNewStylePalette :: ByteString -> Bool
isNewStylePalette kissId = kissId == BS.pack [0x4B, 0x69, 0x53, 0x53]

-- | Return the indexed color, or @#000000@ (black) if the index is out of
-- range.
--
-- >>> fmap (colorByIndex 13) <$> runEitherT (parseKCF palette)
-- Right "#800080"
colorByIndex :: Int -- ^ Index of desired color.
             -> PalEntries
             -> String
colorByIndex index (PalEntries entries) =
    let (PalEntry red green blue) = Safe.atDef (PalEntry 0 0 0) entries index
        paddedHex = Fmt.left 2 '0' %. Fmt.hex
        formatString = "#" % paddedHex % paddedHex % paddedHex
    in Fmt.formatToString formatString red green blue

-- | Return the indexed 'PalEntry', or @PalEntry 0 0 0@ (black) if the index is
-- out of range.
--
-- >>> fmap (palEntryByIndex 16) <$> runEitherT (parseKCF palette)
-- Right (PalEntry {palRed = 193, palGreen = 108, palBlue = 155})
palEntryByIndex :: Int -- ^ Index of desired 'PalEntry'.
                -> PalEntries
                -> PalEntry
palEntryByIndex index (PalEntries entries) =
    Safe.atDef (PalEntry 0 0 0) entries index

-- | Return the number of palette entries.
--
-- Under normal circumstances, there shouldn't be any need to call this
-- function. It's intended for testing purposes only.
lengthPalEntries :: PalEntries -> Int
lengthPalEntries (PalEntries entries) = length entries

-- * Parser

-- | Parse a KiSS palette.
--
-- This is the top-level parser that does the actual work of parsing a KiSS
-- palette.
parsePalette :: HeaderStyle -> BinaryParser PalEntries
parsePalette headerStyle = do
    kcfHeader <- if headerStyle == Old then parseOldHeader else parseHeader
    let numPalGroups = fromIntegral (kcfGroups kcfHeader)
        bpp = kcfBpp kcfHeader
        colors = kcfColors kcfHeader
        parseOnePalGroup = parsePalGroup bpp colors
        parseAllPalGroups = CM.replicateM numPalGroups parseOnePalGroup
    entries <- parseAllPalGroups
    return $ PalEntries (Safe.headDef [] entries)

-- * Palette header

-- | Palette header for both old- and new-styles.
--
-- Not every field from the KiSS specification is here, only the ones that we
-- need to use.
data KCFHeader = KCFHeader
    { kcfBpp    :: !Word8 -- ^ Bits per color (12 or 24).
    , kcfColors :: !Word16 -- ^ No. of colors in one palette group (1-256).
    , kcfGroups :: !Word16 -- ^ No. of palette groups (1-10).
    } deriving (Show)

-- | Old- or new-style header.
data HeaderStyle = Old | New deriving (Eq)

-- | Parse an old-style palette header.
--
-- 12 bits per color, 16 colors in a palette group, and 10 palette groups.
parseOldHeader :: BinaryParser KCFHeader
parseOldHeader = return $ KCFHeader 12 16 10

-- | Parse a new-style palette header.
parseHeader :: BinaryParser KCFHeader
parseHeader = do
    BP.unitOfSize 4 -- Skip the "KiSS" identifier.
    _ <- BP.matchingByte isValidFilemark
    bpp <- BP.matchingByte isValidBpp
    BP.unitOfSize 2 -- Skip the reserved field.
    colors <- BP.leWord16
    groups <- BP.leWord16
    BP.unitOfSize 20 -- Skip the remaining reserved fields (2 + 2 + 16 = 20).
    return $ KCFHeader bpp colors groups

-- | Return 'True' if a new-style palette header has the required filemark
-- (0x10), else an error message with the invalid filemark.
isValidFilemark :: Word8 -> Either Text Bool
isValidFilemark filemark =
    if filemark == 0x10
        then Right True
        else Left errMsg
  where
    formatString = "Filemark should be 0x10 and is actually " % Fmt.prefixHex
    errMsg = Fmt.sformat formatString filemark

-- | Return the given 'Word8' if it's 12 or 24, else an error message with the
-- invalid bits-per-pixel.
isValidBpp :: Word8 -> Either Text Word8
isValidBpp bpp =
    if bpp == 12 || bpp == 24
        then Right bpp
        else Left errMsg
  where
    formatString = "Invalid bits-per-pixel of " % Fmt.int
    errMsg = Fmt.sformat formatString bpp

-- * Palette data

-- | A single palette entry.
--
-- For 12-bit color palettes, each color uses 4 bits.
--
-- For 24-bit color palettes, each color uses 8 bits.
data PalEntry = PalEntry
    { palRed   :: !Word8 -- ^ Red color component.
    , palGreen :: !Word8 -- ^ Green color component.
    , palBlue  :: !Word8 -- ^ Blue color component.
    } deriving (Eq, Show)

-- | Parse a palette group.
parsePalGroup :: Word8 -- ^ Bits per pixel (12 or 24).
              -> Word16 -- ^ No. of colors in a palette group (1-256).
              -> BinaryParser [PalEntry]
parsePalGroup bpp colors =
    BP.sized (fromIntegral groupSize) (CA.some parser)
  where
    -- A 12-bit color palette entry uses 2 bytes; a 24-bit one uses 3.
    (groupSize, parser) =
        if bpp ==12
            then (colors * 2, parse12bpp)
            else (colors * 3, parse24bpp)

-- | Parse a 12-bit color palette entry.
--
-- The colors are stored in 2 bytes:
--
-- > |<-  byte   ->|  |<-  byte   ->|
-- > MSB         LSB  MSB         LSB
-- > | rrrr | bbbb |  | 0000 | gggg | ...
parse12bpp :: BinaryParser PalEntry
parse12bpp = do
    redBlueByte <- BP.byte
    greenByte <- BP.byte
    let -- We multiply each color component (red, green, and blue) by
        -- rgbColorStep in order to map it to the RGB color space. So we have
        -- rgb(0, 0, 0), rgb(17, 17, 17), ..., to rgb(255, 255, 255). Choosing
        -- a slightly lower value for rgbColorStep, like 16 for example, will
        -- create colors that are a shade darker (a 6% difference: 100 - 16 /
        -- 17 * 100 = 5.88).
        rgbColorStep = 17
        red = hiNibble redBlueByte * rgbColorStep
        green = loNibble greenByte * rgbColorStep
        blue = loNibble redBlueByte * rgbColorStep
    return $ PalEntry red green blue

-- | Return the high nibble.
hiNibble :: Word8 -> Word8
hiNibble n = Bits.shiftR n 4 .&. 0x0F

-- | Return the low nibble.
loNibble :: Word8 -> Word8
loNibble n = n .&. 0x0F

-- | Parse a 24-bit color palette entry.
parse24bpp :: BinaryParser PalEntry
parse24bpp = do
    red <- BP.byte
    green <- BP.byte
    blue <- BP.byte
    return $ PalEntry red green blue
