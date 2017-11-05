{-|
Module      : ParseCel
Description : Parse old- and new-style KiSS cels
Copyright   : (c) Libby Horacek, 2015-2017
                  Huggable Monad, 2017
License     : GPL-3
Maintainer  : huggablemonad@hushmail.me
Stability   : experimental
Portability : POSIX

This module provides functions for parsing old- and new-style KiSS cels.

It was ported from the @cel2pnm@ @C@ program written by Libby Horacek.
-}

{-# LANGUAGE OverloadedStrings #-}

module ParseCel
    (
      -- * API
      CelPixels
    , parseCel

      -- * Cel header
    , CelHeader(celWidth, celHeight, celXoffset, celYoffset)
    ) where

import           BinaryParser               (BinaryParser)
import qualified BinaryParser               as BP
import qualified Control.Applicative        as CA
import           Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as ET
import           Data.Bits                  ((.&.))
import qualified Data.Bits                  as Bits
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Word                  (Word16, Word8)
import           Formatting                 ((%))
import qualified Formatting                 as Fmt

-- * API

-- | Cel pixels.
type CelPixels = [Word8]

-- | Parse a KiSS cel.
--
-- For a new-style, 8bpp, 4x4 cel:
--
-- >>> fmap fst <$> ET.runEitherT (parseCel celData)
-- Right (CelHeader {celBpp = 8, celWidth = 4, celHeight = 4, celXoffset = 0, celYoffset = 0})
--
-- >>> fmap snd <$> ET.runEitherT (parseCel celData)
-- Right [128,255,17,23,0,0,40,56,103,11,0,0,0,90,159,238]
parseCel :: ByteString -> EitherT Text IO (CelHeader, CelPixels)
parseCel celData = do
    let headerStyle = if isNewStyleCel (BS.take 4 celData) then New else Old
    ET.hoistEither $ BP.run (parseCelData headerStyle) celData

-- | Return 'True' if the cel is new-style (starts with @KiSS@).
isNewStyleCel :: ByteString -> Bool
isNewStyleCel kissId = kissId == BS.pack [0x4B, 0x69, 0x53, 0x53]

-- * Parser

-- | Parse a KiSS cel.
--
-- This is the top-level parser that does the actual work of parsing a KiSS
-- cel.
--
-- It returns an error if there are trailing bytes (extra cel pixels) in the
-- cel.
parseCelData :: HeaderStyle -> BinaryParser (CelHeader, CelPixels)
parseCelData headerStyle = do
    celHeader <- if headerStyle == Old then parseOldHeader else parseHeader
    let is4bpp = celBpp celHeader == 4
        -- For 4bpp cels, a padding pixel is added if the width is odd. We'll
        -- need to take this extra pixel into account when calculating the
        -- number of bytes to read.
        width4bpp = if odd (celWidth celHeader)
                        then (celWidth celHeader + 1) `div` 2
                        else celWidth celHeader `div` 2
        width8bpp = celWidth celHeader
        width = fromIntegral $ if is4bpp then width4bpp else width8bpp
        height = fromIntegral (celHeight celHeader)
        celSize = width * height
        parser = if is4bpp then parse4bpp (celWidth celHeader) else parse8bpp
    pixels <- BP.bytesOfSize celSize
    let celPixels = parser pixels
    BP.endOfInput
    return (celHeader, celPixels)

-- * Cel header

-- | Cel header for both old- and new-styles.
--
-- Not every field from the KiSS specification is here, only the ones that we
-- need to use.
data CelHeader = CelHeader
    { celBpp     :: !Word8 -- ^ Bits per pixel (4 or 8).
    , celWidth   :: !Word16 -- ^ Cel width (1-XMAX).
    , celHeight  :: !Word16 -- ^ Cel height (1-YMAX) .
    , celXoffset :: !Word16 -- ^ Cel x-offset (0-(XMAX-1)).
    , celYoffset :: !Word16 -- ^ Cel y-offset (0-(YMAX-1)).
    } deriving (Eq, Show)

-- | Old- or new-style header.
data HeaderStyle = Old | New deriving (Eq)

-- | Parse an old-style cel header.
--
-- 4 bits per pixel, x-offset = 0, and y-offset = 0.
parseOldHeader :: BinaryParser CelHeader
parseOldHeader = do
    width <- BP.leWord16
    height <- BP.leWord16
    if isValidWidthAndHeight width height
        then return $ CelHeader 4 width height 0 0
        else BP.failure "Invalid width or height"

-- | Parse a new-style cel header.
parseHeader :: BinaryParser CelHeader
parseHeader = do
    BP.unitOfSize 4 -- Skip the "KiSS" identifier.
    _ <- BP.matchingByte isValidFilemark -- Bail if filemark is invalid.
    bpp <- BP.matchingByte isValidBpp -- Bail if bits-per-pixel is invalid.
    BP.unitOfSize 2 -- Skip the reserved field.
    width <- BP.leWord16
    height <- BP.leWord16
    xOffset <- BP.leWord16
    yOffset <- BP.leWord16
    BP.unitOfSize 16 -- Skip the remaining reserved field.
    if isValidWidthAndHeight width height
        then return $ CelHeader bpp width height xOffset yOffset
        else BP.failure "Invalid width or height"

-- | Return 'True' if a new-style cel header has the required filemark (0x20),
-- else an error message with the invalid filemark.
isValidFilemark :: Word8 -> Either Text Bool
isValidFilemark filemark =
    if filemark == 0x20
        then Right True
        else Left errMsg
  where
    formatString = "Filemark should be 0x20 and is actually " % Fmt.prefixHex
    errMsg = Fmt.sformat formatString filemark

-- | Return the given 'Word8' if it's 4 or 8, else an error message with the
-- invalid bits-per-pixel.
isValidBpp :: Word8 -> Either Text Word8
isValidBpp bpp =
    if bpp == 4 || bpp == 8
        then Right bpp
        else Left errMsg
  where
    formatString = "Invalid bits-per-pixel of " % Fmt.int
    errMsg = Fmt.sformat formatString bpp

-- | Return 'True' if the width and height are valid values, else an error
-- message.
isValidWidthAndHeight :: Word16 -- ^ Width.
                      -> Word16 -- ^ Height.
                      -> Bool
isValidWidthAndHeight width height = width > 0 && height > 0

-- * Pixel data

-- | Parse a 4-bit color pixel.
--
-- The colors are stored in a single byte:
--
-- > |<-  byte   ->|  |<-  byte   ->|  |<-  byte   ->|
-- > MSB         LSB  MSB         LSB  MSB         LSB
-- > | pix0 | pix1 |  | pix2 | pix3 |  | pix4 | pix5 |  ......... | pixN |
parse4bpp :: Word16 -- ^ Cel width.
          -> ByteString -- ^ Cel pixel data.
          -> CelPixels
parse4bpp rasterWidth pixels =
    BSL.unpack $ parse 1 pixels (BS.byteString BS.empty)
  where
    parse xPos pix acc
        | BS.null pix = BS.toLazyByteString acc
        | otherwise =
            let byte = BS.head pix
                rest = BS.tail pix
                hi = BS.word8 . hiNibble $ byte
                lo = BS.word8 . loNibble $ byte
                isPaddingPixel = odd rasterWidth && xPos == width
                acc' = if isPaddingPixel
                           then acc <> hi -- Skip the padding pixel.
                           else acc <> hi <> lo
                xPos' = if xPos == width then 1 else xPos + 1
            in parse xPos' rest acc'
    -- Number of bytes that the raster width of the cel takes up.
    width = if odd rasterWidth
                then (rasterWidth + 1) `div` 2
                else rasterWidth `div` 2

-- | Return the high nibble.
hiNibble :: Word8 -> Word8
hiNibble n = Bits.shiftR n 4 .&. 0x0F

-- | Return the low nibble.
loNibble :: Word8 -> Word8
loNibble n = n .&. 0x0F

-- | Parse an 8-bit color pixel.
parse8bpp :: ByteString -> CelPixels
parse8bpp pixels = BS.unpack pixels
