{-|
Module      : CelToPng
Description : Convert a KiSS cel to a PNG
Copyright   : Huggable Monad, 2017
License     : GPL-3
Maintainer  : huggablemonad@hushmail.me
Stability   : experimental
Portability : POSIX

This module provides functions for converting a KiSS cel to a PNG in RGBA8
format.
-}

module CelToPng
    (
      -- * API
      celToPng
    ) where

import           Codec.Picture        (Image (..), Pixel8, PixelRGBA8 (..))
import qualified Codec.Picture        as Pic
import qualified Data.ByteString.Lazy as BSL

import           ParseCel
import           ParseKCF

-- | Save the cel as a PNG in RGBA8 format.
celToPng :: FilePath -- ^ Output file.
         -> PalEntries
         -> CelHeader
         -> CelPixels
         -> IO ()
celToPng outputFile entries celHeader celPixels =
    case Pic.encodePalettedPng palette image of
        Left _    -> return ()
        Right png -> BSL.writeFile outputFile png
  where
    width = fromIntegral (celWidth celHeader)
    height = fromIntegral (celHeight celHeader)
    image = celToRGBA8 width height celPixels
    palette = palToRGBA8 entries

-- | Return the 'Pixel8' representation of the given cel.
celToRGBA8 :: Int -- ^ Cel width.
           -> Int -- ^ Cel height.
           -> CelPixels
           -> Image Pixel8
celToRGBA8 width height celPixels =
    Pic.generateImage generator width height
  where
    generator x y = pixelByIndex (y * width + x) celPixels

-- | Return the RGBA8 representation of the given palette.
palToRGBA8 :: PalEntries -> Image PixelRGBA8
palToRGBA8 entries = Pic.generateImage generator width height
  where
    width = lengthPalEntries entries
    height = 1
    generator x y =
        let alpha = if x == 0 then 0x00 else 0xFF
            (PalEntry red green blue) =
                palEntryByIndex (fromIntegral x) entries
        in PixelRGBA8 red green blue alpha
