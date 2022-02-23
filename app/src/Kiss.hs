{-# LANGUAGE OverloadedStrings #-}

module Kiss where

import           Data.Aeson hiding (Array)
import           Data.Array (Array)

data KissSet = KissSet { kData    :: CNFKissData
                       , kCnfCels :: [CNFKissCel]
                       , kPalette :: Array Int PaletteFilename
                       } deriving (Eq, Show)

type Palettes = Array Int PaletteFilename

data KissData = KissData { kMemory     :: Int
                         , kBorder     :: Color
                         , kBackground :: Color
                         , kPalettes   :: [PaletteFilename]
                         , kWindowSize :: (Int, Int)
                         , kCels       :: [KissCel]
                         , kPositions  :: [KissSetPos] }
    deriving (Eq, Show)
instance ToJSON KissData where
    toJSON (KissData _ border bg _ win cels pos) =
        object["window_size" .= win,
               "border" .= border,
               "background" .= bg,
               "cels" .= cels,
               "positions" .= pos]

data CNFKissData = CNFKissData { cnfkMemory     :: Int,
                                 cnfkBorder     :: Int,
                                 cnfkPalettes   :: [PaletteFilename],
                                 cnfkWindowSize :: (Int, Int),
                                 cnfkCels       :: [KissCel],
                                 cnfSetPos      :: [KissSetPos]}
    deriving (Eq, Show)

-- The CNF file doesn't have the cel offset (that is stored in the cel file)
data CNFKissCel = CNFKissCel {
  cnfCelMark    :: Int,
  cnfCelFix     :: Int,
  cnfCelName    :: String,
  cnfCelPalette :: Int,
  cnfCelSets    :: [Int],
  cnfCelAlpha   :: Int }
  deriving (Eq, Show)

data KissCel = KissCel {
                    celMark    :: Int,
                    celFix     :: Int,
                    celName    :: String,
                    celPalette :: Int,
                    celSets    :: [Int],
                    celAlpha   :: Int,
                    celOffset  :: SetPos}
    deriving (Eq, Show)
instance ToJSON KissCel where
    toJSON (KissCel mark fix name pal sets alpha offset) =
        object["mark" .= mark,
               "fix" .= fix,
               "name" .= name,
               "palette" .= pal,
               "sets" .= toJSON sets,
               "alpha" .= alpha,
               "offset" .= toJSON offset]

data KissSetPos = KissSetPos {
            setPalette  :: Int,
            setPosition :: [SetPos] }
    deriving (Eq, Show)
instance ToJSON KissSetPos where
    toJSON (KissSetPos pal positions) =
        object["palette" .= pal,
               "positions" .= toJSON positions]

data SetPos = Position {
                setx :: Int,
                sety :: Int }
            | NoPosition
    deriving (Eq, Show)
instance ToJSON SetPos where
    toJSON (Position x y) = object ["x" .= x, "y" .= y]
    toJSON NoPosition     = object ["x" .= (0 :: Int), "y" .= (0 :: Int)]

type PaletteFilename = String
type CelFilename = String
type Color = String
