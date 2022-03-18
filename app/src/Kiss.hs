{-# LANGUAGE OverloadedStrings #-}

module Kiss where

import           Data.Aeson hiding (Array)
import           Data.Array (Array)

data KissDoll = KissDoll { kData    :: CNFKissData
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
                         , kPositions  :: [KissSetPos]
                         , kFKiSS      :: [FKiSSEvent] }
    deriving (Eq, Show)
instance ToJSON KissData where
    toJSON (KissData _ border bg _ win cels pos fkiss) =
        object["window_size" .= win,
               "border" .= border,
               "background" .= bg,
               "cels" .= cels,
               "positions" .= pos,
               "fkiss" .= fkiss]

data CNFKissData = CNFKissData { cnfkMemory     :: Int,
                                 cnfkBorder     :: Int,
                                 cnfkPalettes   :: [PaletteFilename],
                                 cnfkWindowSize :: (Int, Int),
                                 cnfkCels       :: [KissCel],
                                 cnfSetPos      :: [KissSetPos],
                                 cnfFKiSS       :: [FKiSSEvent] }
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

data FKiSSEvent = FKiSSEvent {
    fKiSSEvent     :: String,
    fKiSSEventArgs :: [FKiSSArg],
    fKiSSCommands  :: [FKiSSAction]
} deriving (Eq, Show)
instance ToJSON FKiSSEvent where
    toJSON (FKiSSEvent name args commands) =
        object["event" .= name,
               "args" .= toJSON args,
               "actions" .= toJSON commands ]

data FKiSSArg = Object Int
              | Number Int
              | Text String
    deriving (Eq, Show)
instance ToJSON FKiSSArg where
    toJSON (Kiss.Object mark) = toJSON mark
    toJSON (Kiss.Number n) = toJSON n
    toJSON (Kiss.Text s) = toJSON s

data FKiSSAction = FKiSSAction {
    fKiSSAction     :: String,
    fKiSSActionArgs :: [FKiSSArg]
} deriving (Eq, Show)
instance ToJSON FKiSSAction where
    toJSON (FKiSSAction name args) =
        object["action" .= name,
               "args" .= toJSON args ]

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
