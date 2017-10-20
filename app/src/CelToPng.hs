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

import           Codec.Picture (Image (..), PixelRGBA8 (..))
import qualified Codec.Picture as Pic

import           ParseCel
import           ParseKCF

-- | Save the cel as a PNG in RGBA8 format.
celToPng :: FilePath -- ^ Output file.
         -> PalEntries
         -> CelHeader
         -> CelPixels
         -> IO ()
celToPng outputFile entries celHeader celPixels =
    Pic.writePng outputFile image
    where width = fromIntegral (celWidth celHeader)
          height = fromIntegral (celHeight celHeader)
          image = celToRGBA8 width height entries celPixels

-- | Return the RBGA8 representation of the given cel.
celToRGBA8 :: Int -- ^ Cel width.
           -> Int -- ^ Cel height.
           -> PalEntries
           -> CelPixels
           -> Image PixelRGBA8
celToRGBA8 width height entries celPixels =
    snd $ Pic.generateFoldImage generator celPixels width height
    where generator pixels x y =
              let pixel = head pixels
                  alpha = (if pixel == 0 then 0x00 else 0xFF)
                  (PalEntry red green blue) =
                      palEntryByIndex (fromIntegral pixel) entries
              in (tail pixels, PixelRGBA8 red green blue alpha)
