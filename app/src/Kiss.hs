{-# LANGUAGE OverloadedStrings #-}

module Kiss where

import           Data.Aeson hiding (Array)
import           Data.Array (Array)

data KissSet = KissSet { kData    :: CNFKissData
                       , kCells   :: [CNFKissCell]
                       , kPalette :: Array Int PaletteFilename
                       } deriving (Eq, Show)

type Palettes = Array Int PaletteFilename

data KissData = KissData { kMemory     :: Int
                         , kBorder     :: Color
                         , kBackground :: Color
                         , kPalettes   :: [PaletteFilename]
                         , kWindowSize :: (Int, Int)
                         , kObjects    :: [KissObject]
                         , kCels       :: [KissCell] }
    deriving (Eq, Show)
instance ToJSON KissData where
    toJSON (KissData _ border bg _ win objs cels) =
        object["window_size" .= win,
               "objs" .= objs,
               "border" .= border,
               "background" .= bg,
               "cels" .= cels]

data CNFKissData = CNFKissData { cnfkMemory     :: Int,
                                 cnfkBorder     :: Int,
                                 cnfkPalettes   :: [PaletteFilename],
                                 cnfkWindowSize :: (Int, Int),
                                 cnfkObjects    :: [KissObject] }
    deriving (Eq, Show)

data KissObject = KissObject {
                    objNum   :: Int,
                    objCells :: [KissCell],
                    objPos   :: [SetPos] }
    deriving (Eq, Show)
instance ToJSON KissObject where
    toJSON (KissObject num cells pos) =
        object["id" .= num,
               "cells" .= toJSON cells,
               "positions" .= toJSON pos]

data CNFKissCell = CNFKissCell {
  cnfCelFix     :: Int,
  cnfCelName    :: String,
  cnfCelPalette :: Int,
  cnfCelSets    :: [Int],
  cnfCelAlpha   :: Int }
  deriving (Eq, Show)

data KissCell = KissCell {
                    celFix     :: Int,
                    celName    :: String,
                    celPalette :: Int,
                    celSets    :: [Int],
                    celAlpha   :: Int,
                    celOffset  :: SetPos}
    deriving (Eq, Show)
instance ToJSON KissCell where
    toJSON (KissCell fix name pal sets alpha offset) =
        object["fix" .= fix,
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
    toJSON NoPosition     = object ["x" .= (0 :: Int), "y" .= (0 :: Int) ]

type PaletteFilename = String
type CelFilename = String
type Color = String
