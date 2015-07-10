{-# LANGUAGE OverloadedStrings #-}

module Kiss where

import Data.Aeson

data KissData = KissData {
                    kMemory :: Int,
                    kBorder :: Int,
                    kPalettes :: [String],
                    kWindowSize :: (Int, Int),
                    kObjects :: [KissObject] }
              | BadKiss String
    deriving (Eq, Show)
instance ToJSON KissData where
    toJSON (KissData _ _ _ win objs) =
        object["window_size" .= win,
               "objs" .= objs]
    toJSON (BadKiss s) = toJSON s

data KissObject = KissObject {
                    objNum :: Int,
                    objCells :: [KissCell],
                    objPos :: [SetPos] }
                | BadObject String
    deriving (Eq, Show)
instance ToJSON KissObject where
    toJSON (KissObject num cells pos) =
        object["id" .= num,
               "cells" .= toJSON cells,
               "positions" .= toJSON pos]

data KissCell = KissCell {
                    celFix :: Int,
                    celName :: String,
                    celPalOffset :: Int,
                    celSets :: [Int],
                    celAlpha :: Int }
               | BadCell String
    deriving (Eq, Show)
instance ToJSON KissCell where
    toJSON (KissCell fix name pal sets alpha) =
        object["fix" .= fix, 
               "name" .= name, 
               "palette" .= pal,
               "sets" .= toJSON sets,
               "alpha" .= alpha]       

data KissSet = KissSet {
            setPalette :: Int,
            setPosition :: [SetPos] }
    deriving (Eq, Show)
instance ToJSON KissSet where
    toJSON (KissSet pal positions) =
        object["palette" .= pal,
               "positions" .= toJSON positions]

data SetPos = Position {
                setx :: Int, 
                sety :: Int }
            | NoPosition
    deriving (Eq, Show)
instance ToJSON SetPos where
    toJSON (Position x y) = object["x" .= x, "y" .= y]
    toJSON NoPosition     = "none"

